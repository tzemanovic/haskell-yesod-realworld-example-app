{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Database.Persist.Types.Password.Internal
  ( Password(..)
  , mkPassword
  , verifyPassword
  ) where

import           ClassyPrelude.Yesod
import           Data.Text            (Text)
import qualified Data.Text            as T
import           Database.Persist.Sql (PersistFieldSql (..))
import qualified Yesod.Auth.Util.PasswordStore as PS

-- | Password is stored as a hash.
newtype Password = Password
  { unPassword :: Text }
  deriving (Show, Eq)

-- | Instantiate a 'Password' from 'Text'.
mkPassword :: MonadIO m => Text -> m Password
mkPassword text =
  Password . decodeUtf8 <$> liftIO (PS.makePassword (encodeUtf8 text) 14)

-- | Check a raw password against DB password.
verifyPassword :: Text -> Password -> Bool
verifyPassword rawPassword Password {..} =
  PS.verifyPassword (encodeUtf8 rawPassword) $ encodeUtf8 unPassword

instance PersistField Password where
  toPersistValue Password {..} = PersistText unPassword
  fromPersistValue (PersistText text) = Right $ Password text
  fromPersistValue x =
    Left $
    modulePath <>
    "When trying to deserialize Password: expected PersistText, received: " <>
    T.pack (show x)

instance PersistFieldSql Password where
  sqlType _ = SqlString

instance ToJSON Password where
  toJSON Password {..} = String unPassword

modulePath :: Text
modulePath = "Database/Persist/Types/Password/Internal.hs: "
