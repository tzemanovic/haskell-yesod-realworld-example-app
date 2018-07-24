{-# LANGUAGE NoImplicitPrelude #-}

module Auth
  ( userIdToToken
  )
  where

import           Import
import qualified Auth.JWT as JWT

userIdToToken :: UserId -> HandlerT App IO Text
userIdToToken userId = do
  jwtSecret <- getJwtSecret
  return $ JWT.userIdToToken jwtSecret $ toJSON userId
