{-# LANGUAGE OverloadedStrings #-}

module Web.Forma.Extended
  ( module Web.Forma
    -- * Validation helpers
  , notEmpty
    -- * Running a form
  , withForm
  )
  where


import           Control.Monad.Except      (ExceptT, throwError)
import           Data.Aeson                (object)
import qualified Data.HashMap.Strict       as HM
import qualified Data.Map.Strict           as M
import qualified Data.Text                 as T
import           Import                    hiding (FormResult, foldr, path)
import           Network.HTTP.Types.Status (status422)
import           Prelude                   (foldr)
import           Web.Forma

--------------------------------------------------------------------------------
-- Validation helpers

notEmpty :: Monad m => Text -> ExceptT Text m Text
notEmpty text =
  if T.null text
    then throwError "This field cannot be empty."
    else return text

--------------------------------------------------------------------------------
-- Running a form

-- | Run a form with parsing and validation error handling.

withForm ::
     ToJSON e
  => FormParser names e Handler a
  -> (a -> Handler Value)
  -> Handler Value
withForm form withSucceeded = do
  body <- requireJsonBody :: Handler Value
  r <- runForm form body
  case r of
    Succeeded a ->
      withSucceeded a

    ParsingFailed path msg ->
      sendResponseStatus status400 $ errors $
        maybe val (flip fieldPathToJSON val . unFieldName) path
      where val = String msg

    ValidationFailed verr ->
      sendResponseStatus status422 $ errors $
        concatObjects $
          uncurry fieldPathToJSON . first unFieldName . second toJSON <$>
            M.toAscList verr
  where
    errors e = object [ "errors" .= e ]

--------------------------------------------------------------------------------
-- Helpers

-- | Concatenate JSON objects on their keys.

concatObjects :: [Value] -> Value
concatObjects =
  Object . foldr (HM.unionWith concatValues . unwrap) HM.empty
  where
    unwrap (Object o) = o
    unwrap _          = HM.empty

-- | Concatenate JSON objects\' values.

concatValues :: Value -> Value -> Value
concatValues (Object o1) (Object o2)  = Object $ HM.unionWith concatValues o1 o2
concatValues _           o@(Object _) = o
concatValues o           _            = o

-- | Unroll the field path to JSON that mimics the structure of the input.

fieldPathToJSON :: Foldable t => t Text -> Value -> Value
fieldPathToJSON =
  flip $ foldr (\next acc -> object [next .= acc])
