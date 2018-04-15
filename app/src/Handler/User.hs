{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

module Handler.User (postUsersLoginR, getUserR) where

import Import
import Data.Aeson as J ((.=), object)
import Data.Aeson.TH (deriveJSON, defaultOptions)
import Yesod.Auth.Util.PasswordStore (verifyPassword)

data Creds' = Creds'
  { email :: !Text
  , password :: !Text
  } deriving Show

$(deriveJSON defaultOptions ''Creds')

newtype User' = User'
  { user :: Creds'
  } deriving Show

$(deriveJSON defaultOptions ''User')


postUsersLoginR :: Handler Value
postUsersLoginR = do
  User' {user = Creds' {..}} <- requireJsonBody :: Handler User'
  Just (Entity _ user@User {userPassword = pwdHash}) <-
    runDB $ getBy $ UniqueUserEmail email

  if isPwd password pwdHash
    then encodeUser user
    else unauthorized

getUserR :: Handler Value
getUserR = do
  Just userId <- maybeAuthId
  Just user <- runDB $ get userId

  encodeUser user


isPwd :: Text -> Text -> Bool
isPwd password pwdHash =
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
