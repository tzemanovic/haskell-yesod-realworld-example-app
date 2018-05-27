module Database.Persist.Extended
  ( module Database.Persist
  , maybeUpdate
  ) where

import           Database.Persist


-- | Update a given field if set.

maybeUpdate :: PersistField typ
  => EntityField v typ -> Maybe typ -> [Update v] -> [Update v]
maybeUpdate label mField updates =
  case mField of
    Just field -> (label =. field) : updates
    _          -> updates
