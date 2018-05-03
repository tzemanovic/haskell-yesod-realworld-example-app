{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Text.Digestive.Aeson.Extended
  ( module Text.Digestive
  , module Text.Digestive.Aeson
  , requireValidJson
  , returnValidationErrors
  , nonEmptyText
  , nonEmptyIfSet
  , optionalEmptyText
  , uniqueUsername
  , uniqueEmail
  , validEmail
  , uniqueUsernameIfChanged
  , uniqueEmailIfChanged
  ) where

import qualified Data.Aeson           as J (object)
import qualified Data.Text            as T
import           Import               hiding (Form, check, checkM)
import           Text.Digestive       (Form, Formlet, View, check, checkM, text,
                                       validate, (.:))
import           Text.Digestive.Aeson (digestJSON, jsonErrors)
import qualified Text.Email.Validate  as Email (isValid)


requireValidJson ::  Form v Handler a -> Handler (View v, Maybe a)
requireValidJson form = do
  json <- requireJsonBody :: Handler Value
  digestJSON form json

returnValidationErrors :: ToJSON v => View v -> Handler Value
returnValidationErrors view =
  sendResponseStatus status400 $ J.object ["errors" .= jsonErrors view]

--------------------------------------------------------------------------------

nonEmptyText :: Monad m => Form Text m Text
nonEmptyText =
  check "Can't be empty" (not . null) (text Nothing)

nonEmptyIfSet :: Monad m => Form Text m (Maybe Text)
nonEmptyIfSet =
  check "Can't be empty" (maybe True (not . T.null)) optionalEmptyText

optionalEmptyText :: Monad m => Form Text m (Maybe Text)
optionalEmptyText =
  -- NOTE `text Nothing` returns empty string when not set, this won't work for user update form, because we need to able to distinguish update with empty string from update with the omitted field
  fmap Just (text Nothing)

--------------------------------------------------------------------------------

uniqueUsername :: Form Text Handler Text -> Form Text Handler Text
uniqueUsername =
  checkM "This username is already being used"
    (runDB . (isNothing <$>) . getBy . UniqueUsername)

uniqueEmail :: Form Text Handler Text -> Form Text Handler Text
uniqueEmail =
  checkM "This email is already being used"
    (runDB . (isNothing <$>) . getBy . UniqueUserEmail)

validEmail :: Monad m => Form Text m Text -> Form Text m Text
validEmail =
  check "Invalid email" (Email.isValid . encodeUtf8)

uniqueUsernameIfChanged :: Text -> Form Text Handler (Maybe Text)
uniqueUsernameIfChanged current =
  checkM "This username is already being used" rule nonEmptyIfSet
  where
    rule :: Maybe Text -> Handler Bool
    rule (Just username) = do
          mUser <- runDB $ getBy $ UniqueUsername username
          case mUser of
            Just (Entity _ User {..}) -> return $ userUsername == current
            Nothing                   -> return True

    rule _ = return True

uniqueEmailIfChanged :: Text -> Form Text Handler (Maybe Text)
uniqueEmailIfChanged current =
  checkM "This email is already being used" rule nonEmptyIfSet
  where
    rule :: Maybe Text -> Handler Bool
    rule (Just email) = do
          mUser <- runDB $ getBy $ UniqueUserEmail email
          case mUser of
            Just (Entity _ User {..}) -> return $ userEmail == current
            Nothing                   -> return True

    rule _ = return True
