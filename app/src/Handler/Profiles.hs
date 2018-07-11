module Handler.Profiles
  ( getProfilesR
  , postFollowR
  , deleteFollowR
  , encodeProfile
  ) where

import           Data.Aeson
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
  return $ object ["profile" .= encodeProfile user following]

encodeProfile :: User -> Bool -> Value
encodeProfile User {..} following =
  object
    [ "username" .= userUsername
    , "bio" .= userBio
    , "image" .= userImage
    , "following" .= following
    ]

--------------------------------------------------------------------------------
-- Follow User

postFollowR :: Text -> Handler Value
postFollowR username = do
  (Entity userId _) <- runDB $ getBy404 $ UniqueUserUsername username
  Just currentUserId <- maybeAuthId
  let follower = UserFollower userId currentUserId
  _ <- runDB $ insertUnique follower
  getProfilesR username

--------------------------------------------------------------------------------
-- Unfollow User

deleteFollowR :: Text -> Handler Value
deleteFollowR username = do
  (Entity userId _) <- runDB $ getBy404 $ UniqueUserUsername username
  Just currentUserId <- maybeAuthId
  runDB $ deleteBy $ UniqueUserFollower userId currentUserId
  getProfilesR username
