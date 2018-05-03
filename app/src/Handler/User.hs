{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Handler.User
  (getUserR, putUserR, postUsersLoginR, postUsersRegisterR)
  where

import qualified Data.Aeson                    as J (object)
import           Database.Persist.Extended
import           Import                        hiding (Form, (.:))
import           Text.Digestive.Aeson.Extended
import           Yesod.Auth.Util.PasswordStore (verifyPassword)


data Update' = Update'
  { updateUsername :: Maybe Text
  , updateEmail    :: Maybe Text
  , updatePassword :: Maybe Text
  , updateImage    :: Maybe Text
  , updateBio      :: Maybe Text
  } deriving Show

data Register = Register
  { registerUsername :: !Text
  , registerEmail    :: !Text
  , registerPassword :: !Text
  } deriving Show

data Login = Login
  { loginEmail    :: !Text
  , loginPassword :: !Text
  } deriving Show

--------------------------------------------------------------------------------

getUserR :: Handler Value
getUserR = do
  Just userId <- maybeAuthId
  Just user <- runDB $ get userId
  encodeUser user

putUserR :: Handler Value
putUserR = do
  Just userId <- maybeAuthId
  Just user <- runDB $ get userId
  (view, mUpdate) <- requireValidJson (updateForm user)
  case mUpdate of

    Just Update' {..} -> do
      let updates =
            addUpdateNotNull UserUsername updateUsername $
            addUpdateNotNull UserEmail    updateEmail    $
            addUpdateNotNull UserPassword updatePassword $
            addUpdate        UserImage    updateImage    $
            addUpdate        UserBio      updateBio      []

      updatedUser <- runDB $ updateGet userId updates
      encodeUser updatedUser

    _ -> returnValidationErrors view

postUsersRegisterR :: Handler Value
postUsersRegisterR = do
  (view, mRegister) <- requireValidJson registerForm
  case mRegister of

    Just Register {..} -> do
      let user = User registerEmail registerUsername registerPassword ""
                      defaultUserImage
      _ <- runDB $ insert user
      encodeUser user

    _ -> returnValidationErrors view

postUsersLoginR :: Handler Value
postUsersLoginR = do
  (view, mLogin) <- requireValidJson loginForm
  case mLogin of

    Just Login {..} -> do
      mUser <- runDB $ getBy $ UniqueUserEmail loginEmail
      case mUser of

        Just (Entity _ user@User {userPassword = pwdHash}) | validPwd ->
          encodeUser user
          where validPwd = verifyPwd loginPassword pwdHash

        _ -> unauthorized

    _ -> returnValidationErrors view

--------------------------------------------------------------------------------

updateForm :: User -> Form Text Handler Update'
updateForm User {..} = "user" .: userUpdate
  where
    username = uniqueUsernameIfChanged userUsername
    email = uniqueEmailIfChanged userEmail
    userUpdate = Update' <$> "username" .: username
                         <*> "email"    .: email
                         <*> "password" .: nonEmptyIfSet
                         <*> "image"    .: optionalEmptyText
                         <*> "bio"      .: optionalEmptyText

registerForm :: Form Text Handler Register
registerForm = "user" .: register
  where
    register = Register <$> "username" .: uniqueUsername nonEmptyText
                        <*> "email"    .: uniqueEmail (validEmail nonEmptyText)
                        <*> "password" .: nonEmptyText

loginForm :: Monad m => Form Text m Login
loginForm = "user" .: login
  where
    login = Login <$> "email"    .: nonEmptyText
                  <*> "password" .: nonEmptyText

--------------------------------------------------------------------------------

defaultUserImage :: Text
defaultUserImage = "https://static.productionready.io/images/smiley-cyrus.jpg"

verifyPwd :: Text -> Text -> Bool
verifyPwd password pwdHash =
  verifyPassword (encodeUtf8 password) $ encodeUtf8 pwdHash

unauthorized :: Handler Value
unauthorized = sendResponseStatus status401 Null

encodeUser :: User -> Handler Value
encodeUser User {..} = do
  token <- usernameToJwtToken userUsername
  return $ J.object
    [ "user" .= J.object
        [ "email" .= userEmail
        , "username" .= userUsername
        , "token" .= token
        , "bio" .= userBio
        , "image" .= userImage
        ]
    ]
