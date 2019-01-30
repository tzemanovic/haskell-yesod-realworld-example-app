{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Handler.UserSpec (spec) where

import           Data.Aeson
import qualified Data.CaseInsensitive                  as CI
import           Database.Persist.Types.Email.Internal (Email (..))
import           TestImport


data User' = User'
  { loginEmail    :: Text
  , loginUsername :: Text
  , loginToken    :: Text
  , loginBio      :: Text
  , loginImage    :: Text
  } deriving Show

instance FromJSON User' where
  parseJSON (Object v) = do
    Object u <- v .: "user"
    User' <$> u .: "email"
          <*> u .: "username"
          <*> u .: "token"
          <*> u .: "bio"
          <*> u .: "image"
  parseJSON _ = mzero

spec :: Spec
spec = withApp $ do
    let username = "test"
        rawEmail = "test@foo.com"
        email = Email $ CI.mk rawEmail
        password = "secret"

    describe "postUsersLoginR" $ do

      it "non-existing user can't login" $ do
        postBody UsersLoginR $ encode $ object
          [ "user" .= object
            [ "email" .= email
            , "password" .= password
            ]
          ]
        statusIs 401

      it "user can't login with wrong password" $ do
        void $ insertUser username email password
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
            , "fanny" .= ("wrong" :: Text)
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
        void $ insertUser username email password
        postBody UsersLoginR $ encode $ object
          [ "user" .= object
            [ "email" .= email
            , "password" .= password
            ]
          ]

        statusIs 200
        User' {..} <- getJsonResponse
        assertEq "response email matches" loginEmail rawEmail
        assertEq "response username matches" loginUsername username
        assertNotEq "response token not empty" loginToken ""

    describe "postUsersRegisterR" $ do

      it "user can't register with a duplicate username" $ do
        void $ insertUser username email password
        postBody UsersRegisterR $ encode $ object
          [ "user" .= object
            [ "username" .= ("foo" :: Text)
            , "email" .= email
            , "password" .= password
            ]
          ]
        statusIs 422

      it "user can't register with an invalid email" $ do
        void $ insertUser username email password
        postBody UsersRegisterR $ encode $ object
          [ "user" .= object
            [ "username" .= username
            , "email" .= ("foo" :: Text)
            , "password" .= password
            ]
          ]
        statusIs 422

      it "user can't register with a duplicate email" $ do
        void $ insertUser username email password
        postBody UsersRegisterR $ encode $ object
          [ "user" .= object
            [ "username" .= username
            , "email" .= ("foo@bar.com" :: Text)
            , "password" .= password
            ]
          ]
        statusIs 422

      it "user can register" $ do
        postBody UsersRegisterR $ encode $ object
          [ "user" .= object
            [ "username" .= username
            , "email" .= email
            , "password" .= password
            ]
          ]

        statusIs 200
        User' {..} <- getJsonResponse
        assertEq "response email matches" loginEmail rawEmail
        assertEq "response username matches" loginUsername username
        assertNotEq "response token not empty" loginToken ""

        mUser <- runDB $ getBy $ UniqueUserEmail email
        case mUser of
          Just (Entity _ User {..}) -> do
            assertEq "DB email matches" userEmail email
            assertEq "DB username matches" userUsername username
          _ ->
            liftIO $ assertFailure "user not found in the DB"

    describe "getUserR" $ do

      it "get current user is forbidden when not authenticated" $ do
        get UserR
        statusIs 403

      it "get current user" $ do
        userId <- insertUser username email password
        authenticatedRequest userId $ do
          setMethod "GET"
          setUrl UserR
        statusIs 200

    describe "putUserR" $ do

      it "can't update user with a duplicate email" $ do
        let otherUsername = "taken" :: Text
            otherRawEmail = "taken@bar.com" :: Text
            otherEmail = Email $ CI.mk otherRawEmail
            otherPassword = "something" :: Text
        userId <- insertUser username email password
        void $ insertUser otherUsername otherEmail otherPassword
        authenticatedRequest userId $ do
          setMethod "PUT"
          setUrl UserR
          setRequestBody $ encode $ object
            [ "user" .= object
              [ "username" .= otherUsername
              ]
            ]
        statusIs 422

      it "update user" $ do
        let newUsername = "new username" :: Text
            newBio = "In id erat non orci commodo lobortis." :: Text
        userId <- insertUser username email password
        authenticatedRequest userId $ do
          setMethod "PUT"
          setUrl UserR
          setRequestBody $ encode $ object
            [ "user" .= object
              [ "username" .= newUsername
              , "bio" .= newBio
              ]
            ]
        statusIs 200

        mUser <- runDB $ getBy $ UniqueUserEmail email
        case mUser of
          Just (Entity _ User {..}) -> do
            assertEq "DB username updated" userUsername newUsername
            assertEq "DB bio updated" userBio newBio
          _ ->
            liftIO $ assertFailure "user not found in the DB"
