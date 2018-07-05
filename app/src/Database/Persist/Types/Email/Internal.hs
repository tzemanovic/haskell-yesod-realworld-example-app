{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Database.Persist.Types.Email.Internal
  ( Email(..)
  , mkEmail
  ) where

import           Data.Aeson
import           Data.CaseInsensitive (CI)
import qualified Data.CaseInsensitive as CI
import           Data.Monoid          ((<>))
import           Data.Text            (Text)
import qualified Data.Text            as T
import           Data.Text.Encoding
import           Database.Persist
import           Database.Persist.Sql (PersistFieldSql (..))
import qualified Text.Email.Validate  as Email

-- | Custom Persistance field type that handles email validation.

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
  toPersistValue Email {unEmail = email} = PersistText $ CI.original email
  fromPersistValue (PersistText text) =
    case mkEmail text of
      Just email ->
        Right email
      _ ->
        Left $ path <> "Deserialized invalid email address: " <> text

  fromPersistValue x =
    Left $
    path <>
    "When trying to deserialize Email: expected PersistText, received: " <>
    T.pack (show x)

instance PersistFieldSql Email where
  sqlType _ = SqlString

instance ToJSON Email where
  toJSON Email {..} = String $ CI.original unEmail

path :: Text
path = "Database/Persist/Types/Email/Internal.hs: "
