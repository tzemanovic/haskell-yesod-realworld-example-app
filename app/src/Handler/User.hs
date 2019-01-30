{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Handler.User
  ( postUsersLoginR
  , postUsersRegisterR
  , getUserR
  , putUserR
  ) where

import           Control.Monad.Except      (ExceptT, throwError)
import           Data.Aeson                (object)
import           Database.Persist.Extended
import           Import                    hiding (FormResult)
import           Web.Forma.Extended


--------------------------------------------------------------------------------
-- User login

type LoginFields = '[ "user", "email", "password" ]

data Login = Login
  { loginEmail    :: Email
  , loginPassword :: Text
  } deriving Show

loginForm :: Monad m => FormParser LoginFields Text m Login
loginForm =
  subParser #user (Login
    <$> field #email (notEmpty >=> validEmail)
    <*> field #password notEmpty)

postUsersLoginR :: Handler Value
postUsersLoginR =
  withForm loginForm $ \Login {..} -> do
    mUser <- runDB $ getBy $ UniqueUserEmail loginEmail
    case mUser of

      Just (Entity userId user@User {..}) | validPwd ->
        encodeUser userId user
        where validPwd = verifyPassword loginPassword userPassword

      _ ->
        notAuthenticated

--------------------------------------------------------------------------------
-- Register new user

type RegisterFields = '[ "user", "username", "email", "password" ]

data Register = Register
  { registerUsername :: Text
  , registerEmail    :: Email
  , registerPassword :: Text
  } deriving Show

registerForm :: FormParser RegisterFields Text Handler Register
registerForm =
  subParser #user (Register
    <$> field #username (notEmpty >=> uniqueUsername)
    <*> field #email (notEmpty >=> validEmail >=> uniqueEmail)
    <*> field #password notEmpty)

postUsersRegisterR :: Handler Value
postUsersRegisterR =
  withForm registerForm $ \Register {..} -> do
    pwdHash <- mkPassword registerPassword
    now <- liftIO getCurrentTime
    let user = User registerEmail registerUsername pwdHash "" defaultUserImage
                    now now
    userId <- runDB $ insert user
    encodeUser userId user

--------------------------------------------------------------------------------
-- Get current user

getUserR :: Handler Value
getUserR = do
  mUserId <- maybeAuthId
  case mUserId of
    Nothing -> notAuthenticated
    Just userId -> do
      mUser <- runDB $ get userId
      case mUser of
        Nothing   -> notAuthenticated
        Just user -> encodeUser userId user

--------------------------------------------------------------------------------
-- Update current user

type UpdateFields = '[ "user", "username", "email", "password", "image", "bio" ]

data Update' = Update'
  { updateUsername :: Maybe Text
  , updateEmail    :: Maybe Email
  , updatePassword :: Maybe Text
  , updateImage    :: Maybe Text
  , updateBio      :: Maybe Text
  } deriving Show

updateForm :: User -> FormParser UpdateFields Text Handler Update'
updateForm User {..} =
  subParser #user (Update'
    <$> optional (field #username usernameValidation)
    <*> optional (field #email emailValidation)
    <*> optional (field #password notEmpty)
    <*> optional (field' #image)
    <*> optional (field' #bio))
  where
    usernameValidation = notEmpty >=> uniqueUsernameIfChanged userUsername
    emailValidation = notEmpty >=> validEmail >=> uniqueEmailIfChanged userEmail

putUserR :: Handler Value
putUserR = do
  mUserId <- maybeAuthId
  case mUserId of
    Nothing -> notAuthenticated
    Just userId -> do
      mUser <- runDB $ get userId
      case mUser of
        Nothing -> notAuthenticated
        Just user ->
          withForm (updateForm user) $ \Update' {..} -> do
            now <- liftIO getCurrentTime
            pwdHash <-
              case updatePassword of
                Just pwd -> Just <$> mkPassword pwd
                _        -> return Nothing
            let updates =
                  catMaybes
                    [ maybeUpdate UserUsername updateUsername
                    , maybeUpdate UserEmail updateEmail
                    , maybeUpdate UserPassword pwdHash
                    , maybeUpdate UserImage updateImage
                    , maybeUpdate UserBio updateBio
                    , maybeUpdate UserUpdatedAt (Just now)
                    ]
            updatedUser <- runDB $ updateGet userId updates
            encodeUser userId updatedUser

--------------------------------------------------------------------------------
-- Input validations

validEmail :: Monad m => Text -> ExceptT Text m Email
validEmail email =
  case mkEmail email of
     Just e -> return e
     _      -> throwError "Invalid email address."

uniqueEmail :: Email -> ExceptT Text Handler Email
uniqueEmail email = do
  user <- lift $ runDB $ getBy $ UniqueUserEmail email
  if isNothing user
    then return email
    else throwError "This email address is already being used."

uniqueUsername :: Text -> ExceptT Text Handler Text
uniqueUsername username = do
  user <- lift $ runDB $ getBy $ UniqueUserUsername username
  if isNothing user
    then return username
    else throwError "This username is already being used."

uniqueEmailIfChanged :: Email -> Email -> ExceptT Text Handler Email
uniqueEmailIfChanged currentEmail newEmail =
  if newEmail /= currentEmail
    then uniqueEmail newEmail
    else return newEmail

uniqueUsernameIfChanged :: Text -> Text -> ExceptT Text Handler Text
uniqueUsernameIfChanged currentUsername newUsername =
  if newUsername /= currentUsername
    then uniqueUsername newUsername
    else return newUsername

--------------------------------------------------------------------------------
-- Helpers

defaultUserImage :: Text
defaultUserImage = "https://static.productionready.io/images/smiley-cyrus.jpg"

-- | Encode a 'User' with a JWT authentication token.
encodeUser :: UserId -> User -> Handler Value
encodeUser userId User {..} = do
  token <- userIdToToken userId
  return $ object
    [ "user" .= object
        [ "email" .= userEmail
        , "username" .= userUsername
        , "token" .= token
        , "bio" .= userBio
        , "image" .= userImage
        , "createdAt" .= userCreatedAt
        , "updatedAt" .= userUpdatedAt
        ]
    ]
