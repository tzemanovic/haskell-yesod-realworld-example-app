module Pagination
  ( Page(..)
  , lookupPageParams
  , paginate
  ) where

import           ClassyPrelude.Yesod
import           Data.Text.Read      (decimal)
import           Database.Esqueleto  (Esqueleto, limit, offset)
import           Foundation


data Page = Page
  { pageLimit  :: !Int64
  , pageOffset :: !Int64
  }

lookupPageParams :: Handler Page
lookupPageParams = do
  mOffset <- lookupGetParam "offset"
  mLimit <- lookupGetParam "limit"
  return
    Page
      { pageLimit = decimalWithDefault mLimit 20
      , pageOffset = decimalWithDefault mOffset 0
      }

paginate :: Esqueleto m expr backend => Page -> m ()
paginate Page {..} = do
  limit pageLimit
  offset pageOffset

--------------------------------------------------------------------------------

decimalWithDefault :: Integral p => Maybe Text -> p -> p
decimalWithDefault x default' =
  case decimal <$> x of
    Just (Right (l, _)) -> l
    _                   -> default'
