module Database.Persist.Extended
  ( module Database.Persist
  , module Database.Persist.Types.Email
  , maybeUpdate
  ) where

import           ClassyPrelude.Yesod
import           Database.Persist
import           Database.Persist.Types.Email


-- | Update a given field if set.
maybeUpdate :: PersistField typ
  => EntityField v typ -> Maybe typ -> Maybe (Update v)
maybeUpdate label =
  fmap (label =.)
