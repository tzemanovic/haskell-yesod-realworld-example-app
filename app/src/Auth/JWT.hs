{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Auth.JWT
  ( lookupToken
  , userIdToToken
  , tokenToUserId
  )
  where

import           ClassyPrelude.Yesod
import           Data.Char           (isSpace)
import           Data.Map             as Map (fromList, (!?))
import           Web.JWT              as JWT

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

userIdToToken :: Text -> Value -> Text
userIdToToken jwtSecret userId =
  encodeSigned HS256 (JWT.secret jwtSecret)
    def {unregisteredClaims = Map.fromList [("userId", userId)]}

tokenToUserId :: Text -> Text -> Maybe Value
tokenToUserId jwtSecret token = do
    jwt <- JWT.decodeAndVerifySignature (JWT.secret jwtSecret) token
    JWT.unregisteredClaims (JWT.claims jwt) !? "userId"
