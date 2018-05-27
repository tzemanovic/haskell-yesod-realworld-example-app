module Database.Persist.Extended
  ( module Database.Persist
  , maybeUpdate
  ) where

import           Database.Persist


-- | Update a given field if set.

maybeUpdate :: PersistField typ
  => EntityField v typ -> Maybe typ -> Maybe (Update v)
maybeUpdate label =
  fmap (label =.)
