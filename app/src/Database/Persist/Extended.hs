module Database.Persist.Extended
  ( module Database.Persist
  , addUpdate
  , addUpdateNotNull
  ) where

import           Data.Text        as T
import           Database.Persist


addUpdate :: PersistField typ
  => EntityField v typ -> Maybe typ -> [Update v] -> [Update v]
addUpdate label mField updates =
  case mField of
    Just field -> (label =. field) : updates
    _          -> updates

addUpdateNotNull :: EntityField v Text -> Maybe Text -> [Update v] -> [Update v]
addUpdateNotNull label mField updates =
  case mField of
    Just field | not (T.null field) -> (label =. field) : updates
    _                               -> updates
