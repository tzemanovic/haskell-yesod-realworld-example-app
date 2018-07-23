{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Auth
  ( lookupToken
  )
  where

import           ClassyPrelude.Yesod
import           Data.Char           (isSpace)

-- | Try to lookup token from the Authorization header
lookupToken :: MonadHandler m => m (Maybe Text)
lookupToken = do
  mAuth <- lookupHeader "Authorization"
  return $ extractToken . decodeUtf8 =<< mAuth

extractToken :: Text -> Maybe Text
extractToken auth
  | toLower x == "token" = Just $ dropWhile isSpace y
  | otherwise            = Nothing
  where (x, y) = break isSpace auth
