{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.UserSpec (spec) where

import           Data.Aeson                    (encode, object, toJSON, (.=))
import           TestImport
import           Yesod.Auth.Util.PasswordStore (makePassword)

spec :: Spec
spec = withApp $
    describe "postUsersLoginR" $ do
      let email = "test@foo.com"
          password = "secret"

      it "non-existing user can't login" $ do
        postBody UsersLoginR $ encode $ object
          [ "user" .= object
            [ "email" .= email
            , "password" .= password
            ]
          ]
        statusIs 401

      it "user can't login with wrong password" $ do
        insertUser email password
        postBody UsersLoginR $ encode $ object
          [ "user" .= object
            [ "email" .= email
            , "password" .= ("wrong" :: Text)
            ]
          ]
        statusIs 401

      it "invalid request body fails parsing" $ do
        postBody UsersLoginR $ encode $ object
          [ "user" .= object
            [ "email" .= email
            , "passwor" .= ("wrong" :: Text)
            ]
          ]
        statusIs 400

      it "empty password fails validation" $ do
        postBody UsersLoginR $ encode $ object
          [ "user" .= object
            [ "email" .= email
            , "password" .= ("" :: Text)
            ]
          ]
        statusIs 422

      it "user can login with valid credentials" $ do
        insertUser email password
        postBody UsersLoginR $ encode $ object
          [ "user" .= object
            [ "email" .= email
            , "password" .= password
            ]
          ]
        statusIs 200
