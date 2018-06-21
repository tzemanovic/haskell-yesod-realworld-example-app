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
  )
  where

import           Control.Monad.Except          (ExceptT, throwError)
import           Data.Aeson                    (object)
import           Database.Persist.Extended
import           Import                        hiding (Form, FormResult)
import           Web.Forma.Extra

import           Yesod.Auth.Util.PasswordStore (makePassword, verifyPassword)


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

      Just (Entity _ user@User {userPassword = pwdHash}) | validPwd ->
        encodeUser user
        where validPwd = verifyPwd loginPassword pwdHash

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
    pwdHash <- liftIO $ makePassword (encodeUtf8 registerPassword) 14
    now <- liftIO getCurrentTime
    let user = User registerEmail registerUsername (decodeUtf8 pwdHash) ""
                    defaultUserImage now now
    _ <- runDB $ insert user
    encodeUser user

--------------------------------------------------------------------------------
-- Get current user

getUserR :: Handler Value
getUserR = do
  Just userId <- maybeAuthId
  Just user <- runDB $ get userId
  encodeUser user

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
  Just userId <- maybeAuthId
  Just user <- runDB $ get userId
  withForm (updateForm user) $ \Update' {..} -> do
    now <- liftIO getCurrentTime
    let updates =
          catMaybes
            [ maybeUpdate UserUsername updateUsername
            , maybeUpdate UserEmail updateEmail
            , maybeUpdate UserPassword updatePassword
            , maybeUpdate UserImage updateImage
            , maybeUpdate UserBio updateBio
            , maybeUpdate UserUpdatedAt (Just now)
            ]
    updatedUser <- runDB $ updateGet userId updates
    encodeUser updatedUser

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

verifyPwd :: Text -> Text -> Bool
verifyPwd password pwdHash =
  verifyPassword (encodeUtf8 password) $ encodeUtf8 pwdHash

-- | Encode a 'User' with a JWT authentication token.

encodeUser :: User -> Handler Value
encodeUser User {..} = do
  token <- usernameToJwtToken userUsername
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
