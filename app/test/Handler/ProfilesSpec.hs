{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

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

      it "trying to find non-existing user profile fails gracefully" $ do
        get $ ProfilesR username
        statusIs 404

      it "get user profile" $ do
        _ <- insertUser username email password
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
        let otherUsername = "taken" :: Text
            otherRawEmail = "taken@bar.com" :: Text
            otherEmail = Email $ CI.mk otherRawEmail
            otherPassword = "something" :: Text
        userId <- insertUser username email password
        otherUserId <- insertUser otherUsername otherEmail otherPassword
        _ <- runDB $ insert UserFollower
              { userFollowerUser = otherUserId
              , userFollowerFollower = userId
              }
        authenticatedRequest username $ do
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
