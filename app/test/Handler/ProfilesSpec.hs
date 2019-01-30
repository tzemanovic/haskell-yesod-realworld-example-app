{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler.ProfilesSpec (spec) where

import           Data.Aeson
import qualified Data.CaseInsensitive                  as CI
import           Database.Persist.Types.Email.Internal (Email (..))
import           TestImport

spec :: Spec
spec = withApp $ do
    let username = "test"
        rawEmail = "test@foo.com"
        email = Email $ CI.mk rawEmail
        password = "secret"

    describe "getProfilesR" $ do

      it "trying to get non-existing user profile fails gracefully" $ do
        get $ ProfilesR username
        statusIs 404

      it "get user profile" $ do
        void $ insertUser username email password
        get $ ProfilesR username
        statusIs 200
        response <- getJsonResponse
        assertEq "response contains profile data" response $ object
          [ "profile" .= object
              [ "username" .= username
              , "bio" .= ("" :: Text)
              , "image" .= ("" :: Text)
              , "following" .= False
              ]
          ]

      it "get user profile that indicates following" $ do
        let otherUsername = "lambda" :: Text
            otherRawEmail = "lambda@bar.com" :: Text
            otherEmail = Email $ CI.mk otherRawEmail
            otherPassword = "something" :: Text
        userId <- insertUser username email password
        otherUserId <- insertUser otherUsername otherEmail otherPassword
        void $ runDB $ insert UserFollower
              { userFollowerUser = otherUserId
              , userFollowerFollower = userId
              }
        authenticatedRequest userId $ do
          setMethod "GET"
          setUrl $ ProfilesR otherUsername

        statusIs 200
        response <- getJsonResponse
        assertEq "response contains profile data" response $ object
          [ "profile" .= object
              [ "username" .= otherUsername
              , "bio" .= ("" :: Text)
              , "image" .= ("" :: Text)
              , "following" .= True
              ]
          ]

    describe "postFollowR" $ do

      it "trying to follow non-existing user profile fails gracefully" $ do
        userId <- insertUser username email password
        authenticatedRequest userId $ do
          setMethod "POST"
          setUrl $ FollowR "lambda"
        statusIs 404

      it "following is applied" $ do
        let otherUsername = "lambda" :: Text
            otherRawEmail = "lambda@bar.com" :: Text
            otherEmail = Email $ CI.mk otherRawEmail
            otherPassword = "something" :: Text
        userId <- insertUser username email password
        otherUserId <- insertUser otherUsername otherEmail otherPassword
        void $ runDB $ insert UserFollower
              { userFollowerUser = otherUserId
              , userFollowerFollower = userId
              }
        authenticatedRequest userId $ do
          setMethod "POST"
          setUrl $ FollowR otherUsername

        statusIs 200
        response <- getJsonResponse
        assertEq "response contains profile data" response $ object
          [ "profile" .= object
              [ "username" .= otherUsername
              , "bio" .= ("" :: Text)
              , "image" .= ("" :: Text)
              , "following" .= True
              ]
          ]

    describe "deleteFollowR" $ do

      it "trying to unfollow non-existing user profile fails gracefully" $ do
        userId <- insertUser username email password
        authenticatedRequest userId $ do
          setMethod "DELETE"
          setUrl $ FollowR "lambda"
        statusIs 404

      it "unfollowing is applied" $ do
        let otherUsername = "lambda" :: Text
            otherRawEmail = "lambda@bar.com" :: Text
            otherEmail = Email $ CI.mk otherRawEmail
            otherPassword = "something" :: Text
        userId <- insertUser username email password
        otherUserId <- insertUser otherUsername otherEmail otherPassword
        void $ runDB $ insert UserFollower
              { userFollowerUser = otherUserId
              , userFollowerFollower = userId
              }
        authenticatedRequest userId $ do
          setMethod "DELETE"
          setUrl $ FollowR otherUsername

        statusIs 200
        response <- getJsonResponse
        assertEq "response contains profile data" response $ object
          [ "profile" .= object
              [ "username" .= otherUsername
              , "bio" .= ("" :: Text)
              , "image" .= ("" :: Text)
              , "following" .= False
              ]
          ]
