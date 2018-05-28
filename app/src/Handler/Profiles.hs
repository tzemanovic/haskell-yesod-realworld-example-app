{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Handler.Profiles
  ( getProfilesR
  )
  where

import           Data.Aeson
import           Import


getProfilesR :: Text -> Handler Value
getProfilesR username = do
  (Entity userId user) <- runDB $ getBy404 $ UniqueUserUsername username
  mCurrentUserId <- maybeAuthId
  following <-
    case mCurrentUserId of

      Just currentUserId ->
        isJust <$> runDB (getBy $ UniqueUserFollower userId currentUserId)

      _ ->
        return False

  return $ encodeProfile user following

encodeProfile :: User -> Bool -> Value
encodeProfile User {..} following =
  object
    [ "profile" .= object
        [ "username" .= userUsername
        , "bio" .= userBio
        , "image" .= userImage
        , "following" .= following
        ]
    ]

