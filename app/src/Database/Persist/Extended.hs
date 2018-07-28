{-# LANGUAGE NoImplicitPrelude #-}

module Database.Persist.Extended
  ( module Database.Persist
  , module Database.Persist.Types.Email
  , module Database.Persist.Types.Password
  , maybeUpdate
  ) where

import           ClassyPrelude.Yesod
import           Database.Persist
import           Database.Persist.Types.Email
import           Database.Persist.Types.Password


-- | Update a given field if the given value is set.
maybeUpdate :: PersistField typ
  => EntityField v typ -> Maybe typ -> Maybe (Update v)
maybeUpdate label =
  fmap (label =.)
