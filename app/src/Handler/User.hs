{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Handler.User
  (getUserR, putUserR, postUsersLoginR, postUsersRegisterR)
  where

import           Control.Monad.Except          (ExceptT, throwError)
import           Data.Aeson                    (object)
import qualified Data.HashMap.Strict           as HM
import qualified Data.Map.Strict               as M
import qualified Data.Text                     as T
import           Database.Persist.Extended
import           Import                        hiding (Form, foldr, (.:))
import           Network.HTTP.Types.Status     (status422)
import           Prelude                       (foldr)
import           Text.Digestive.Aeson.Extended
import           Web.Forma                     (FormParser, FormResult (..),
                                                field, runForm, subParser,
                                                unFieldName)
import           Yesod.Auth.Util.PasswordStore (makePassword, verifyPassword)

-- TODO replace with forma
data Update' = Update'
  { updateUsername :: Maybe Text
  , updateEmail    :: Maybe Text
  , updatePassword :: Maybe Text
  , updateImage    :: Maybe Text
  , updateBio      :: Maybe Text
  } deriving Show

data Register = Register
  { registerUsername :: !Text
  , registerEmail    :: !Text
  , registerPassword :: !Text
  } deriving Show

type LoginFields = '["user", "email", "password"]

data Login = Login
  { loginEmail    :: Text
  , loginPassword :: Text
  } deriving Show

loginForm :: Monad m => FormParser LoginFields Text m Login
loginForm =
  subParser #user (Login
    <$> field #email notEmpty
    <*> field #password notEmpty)

--------------------------------------------------------------------------------

getUserR :: Handler Value
getUserR = do
  Just userId <- maybeAuthId
  Just user <- runDB $ get userId
  encodeUser user

putUserR :: Handler Value
putUserR = do
  Just userId <- maybeAuthId
  Just user <- runDB $ get userId
  (view, mUpdate) <- requireValidJson (updateForm user)
  case mUpdate of

    Just Update' {..} -> do
      let updates =
            addUpdateNotNull UserUsername updateUsername $
            addUpdateNotNull UserEmail    updateEmail    $
            addUpdateNotNull UserPassword updatePassword $
            addUpdate        UserImage    updateImage    $
            addUpdate        UserBio      updateBio      []

      updatedUser <- runDB $ updateGet userId updates
      encodeUser updatedUser

    _ -> returnValidationErrors view

postUsersRegisterR :: Handler Value
postUsersRegisterR = do
  (view, mRegister) <- requireValidJson registerForm
  case mRegister of

    Just Register {..} -> do
      pwdHash <- lift $ makePassword (encodeUtf8 registerPassword) 14
      let user = User registerEmail registerUsername (decodeUtf8 pwdHash) ""
                      defaultUserImage
      _ <- runDB $ insert user
      encodeUser user

    _ -> returnValidationErrors view

postUsersLoginR :: Handler Value
postUsersLoginR =
  withForm loginForm $ \Login {..} -> do
    mUser <- runDB $ getBy $ UniqueUserEmail loginEmail
    case mUser of

      Just (Entity _ user@User {userPassword = pwdHash}) | validPwd ->
        encodeUser user
        where validPwd = verifyPwd loginPassword pwdHash

      _ -> unauthorized

--------------------------------------------------------------------------------

withForm ::
     ToJSON e
  => FormParser names e Handler a
  -> (a -> Handler Value)
  -> Handler Value
withForm f withSucceeded = do
  body <- requireJsonBody :: Handler Value
  r <- runForm f body
  case r of
    Succeeded value ->
      withSucceeded value

    ParsingFailed path msg ->
      sendResponseStatus status400 $ errors $
        maybe val (flip fieldPathToJSON val . unFieldName) path
      where val = String msg

    ValidationFailed err ->
      sendResponseStatus status422 $ errors $
        concatObjects $
          uncurry fieldPathToJSON . first unFieldName . second toJSON <$>
            M.toAscList err
  where errors e = object [ "errors" .= e ]

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

--------------------------------------------------------------------------------

updateForm :: User -> Form Text Handler Update'
updateForm User {..} = "user" .: userUpdate
  where
    username = uniqueUsernameIfChanged userUsername
    email = uniqueEmailIfChanged userEmail
    userUpdate = Update' <$> "username" .: username
                         <*> "email"    .: email
                         <*> "password" .: nonEmptyIfSet
                         <*> "image"    .: optionalEmptyText
                         <*> "bio"      .: optionalEmptyText

registerForm :: Form Text Handler Register
registerForm = "user" .: register
  where
    register = Register <$> "username" .: uniqueUsername nonEmptyText
                        <*> "email"    .: uniqueEmail (validEmail nonEmptyText)
                        <*> "password" .: nonEmptyText

--------------------------------------------------------------------------------

notEmpty :: Monad m => Text -> ExceptT Text m Text
notEmpty txt =
  if T.null txt
    then throwError "This field cannot be empty."
    else return txt

--------------------------------------------------------------------------------

defaultUserImage :: Text
defaultUserImage = "https://static.productionready.io/images/smiley-cyrus.jpg"

verifyPwd :: Text -> Text -> Bool
verifyPwd password pwdHash =
  verifyPassword (encodeUtf8 password) $ encodeUtf8 pwdHash

unauthorized :: Handler Value
unauthorized = sendResponseStatus status401 Null

encodeUser :: User -> Handler Value
encodeUser User {..} = do
  token <- usernameToJwtToken userUsername
  return $ object
    [ "user" .= object
        [ "email" .= userEmail
        , "username" .= userUsername
        , "token" .= token
        , "bio" .= userBio
        , "image" .= userImage
        ]
    ]
