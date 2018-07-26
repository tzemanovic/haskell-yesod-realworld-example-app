{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Auth.JWT
  ( lookupToken
  , jsonToToken
  , tokenToJson
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

jsonToToken :: Text -> Value -> Text
jsonToToken jwtSecret userId =
  encodeSigned HS256 (JWT.secret jwtSecret)
    def {unregisteredClaims = Map.fromList [(jwtKey, userId)]}

tokenToJson :: Text -> Text -> Maybe Value
tokenToJson jwtSecret token = do
    jwt <- JWT.decodeAndVerifySignature (JWT.secret jwtSecret) token
    JWT.unregisteredClaims (JWT.claims jwt) !? jwtKey

jwtKey :: Text
jwtKey = "jwt"
