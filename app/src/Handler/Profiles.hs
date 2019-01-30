{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler.Profiles
  ( getProfilesR
  , postFollowR
  , deleteFollowR
  ) where

import           Data.Aeson
import qualified Database
import           Import


--------------------------------------------------------------------------------
-- Get Profile

getProfilesR :: Text -> Handler Value
getProfilesR username = do
  (Entity userId user) <- runDB $ getBy404 $ UniqueUserUsername username
  mCurrentUserId <- maybeAuthId
  following <-
    maybe
      (return False)
      (map isJust . runDB . getBy . UniqueUserFollower userId)
      mCurrentUserId
  return $ object ["profile" .= Database.encodeProfile user following]

--------------------------------------------------------------------------------
-- Follow User

postFollowR :: Text -> Handler Value
postFollowR username = do
  (Entity userId _) <- runDB $ getBy404 $ UniqueUserUsername username
  mCurrentUserId <- maybeAuthId
  case mCurrentUserId of
    Nothing -> notFound
    Just currentUserId -> do
      let follower = UserFollower userId currentUserId
      void $ runDB $ insertUnique follower
      getProfilesR username

--------------------------------------------------------------------------------
-- Unfollow User

deleteFollowR :: Text -> Handler Value
deleteFollowR username = do
  (Entity userId _) <- runDB $ getBy404 $ UniqueUserUsername username
  mCurrentUserId <- maybeAuthId
  case mCurrentUserId of
    Nothing -> notFound
    Just currentUserId -> do
      runDB $ deleteBy $ UniqueUserFollower userId currentUserId
      getProfilesR username
