{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Database.Persist.Types.Email.Internal
  ( Email(..)
  , mkEmail
  ) where

import           ClassyPrelude.Yesod
import           Data.CaseInsensitive (CI)
import qualified Data.CaseInsensitive as CI
import           Data.Text            (Text)
import qualified Data.Text            as T
import           Database.Persist.Sql (PersistFieldSql (..))
import qualified Text.Email.Validate  as Email

-- | Email is stored in a case-insensitive string.
newtype Email = Email
  { unEmail :: CI Text }
  deriving (Show, Eq)

-- | Try to instantiate an 'Email' from 'Text'.
mkEmail :: Text -> Maybe Email
mkEmail email =
  if Email.isValid $ encodeUtf8 email
    then Just $ Email $ CI.mk email
    else Nothing

instance PersistField Email where
  toPersistValue Email {..} = PersistText $ CI.original unEmail
  fromPersistValue (PersistText text) =
    case mkEmail text of
      Just email ->
        Right email
      _ ->
        Left $ modulePath <> "Deserialized invalid email address: " <> text

  fromPersistValue x =
    Left $
    modulePath <>
    "When trying to deserialize Email: expected PersistText, received: " <>
    T.pack (show x)

instance PersistFieldSql Email where
  sqlType _ = SqlString

instance ToJSON Email where
  toJSON Email {..} = String $ CI.original unEmail

modulePath :: Text
modulePath = "Database/Persist/Types/Email/Internal.hs: "
