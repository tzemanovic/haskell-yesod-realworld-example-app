{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Handler.User (postUsersLoginR, getUserR, postUsersRegisterR) where

import Import hiding (Form, (.:), check, checkM)
import Text.Digestive as D (View, Form, (.:), check, checkM, text)
import Text.Digestive.Aeson (digestJSON, jsonErrors)
import Yesod.Auth.Util.PasswordStore (verifyPassword)
import qualified Data.Aeson as J (object)
import qualified Text.Email.Validate as Email (isValid)


data Login = Login
  { loginEmail :: !Text
  , loginPassword :: !Text
  } deriving Show

data Register = Register
  { registerUsername :: !Text
  , registerEmail :: !Text
  , registerPassword :: !Text
  } deriving Show

--------------------------------------------------------------------------------

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

getUserR :: Handler Value
getUserR = do
  Just userId <- maybeAuthId
  Just user <- runDB $ get userId
  encodeUser user

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

--------------------------------------------------------------------------------

requireValidJson ::  Form v Handler a -> Handler (View v, Maybe a)
requireValidJson form = do
  json <- requireJsonBody :: Handler Value
  digestJSON form json

returnValidationErrors :: ToJSON v => View v -> Handler Value
returnValidationErrors view =
  sendResponseStatus status400 $ J.object ["errors" .= jsonErrors view]

loginForm :: Monad m => Form Text m Login
loginForm = "user" .: login
  where
    login = Login <$> "email"    .: nonEmptyText
                  <*> "password" .: nonEmptyText

registerForm :: Form Text Handler Register
registerForm = "user" .: register
  where
    register = Register <$> "username" .: uniqueUsername nonEmptyText
                        <*> "email"    .: uniqueEmail (validEmail nonEmptyText)
                        <*> "password" .: nonEmptyText

nonEmptyText :: Monad m => Form Text m Text
nonEmptyText = check "Can't be empty" (not . null) (text Nothing)

uniqueUsername :: Form Text Handler Text -> Form Text Handler Text
uniqueUsername = checkM "This username is already being used"
    (runDB . (isNothing <$>) . getBy . UniqueUsername)

uniqueEmail :: Form Text Handler Text -> Form Text Handler Text
uniqueEmail = checkM "This email is already being used"
    (runDB . (isNothing <$>) . getBy . UniqueUserEmail)

validEmail :: Monad m => Form Text m Text -> Form Text m Text
validEmail = check "Invalid email" (Email.isValid . encodeUtf8)

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
