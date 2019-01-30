{-# LANGUAGE ExplicitForAll        #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ViewPatterns          #-}

module Foundation where

import qualified Auth.JWT             as JWT
import           Data.Aeson           (Result (Success), fromJSON)
import           Database.Persist.Sql (ConnectionPool, runSqlPool)
import           Import.NoFoundation
import qualified Yesod.Auth.Message   as AuthMsg
import           Yesod.Core.Types     (Logger)
import qualified Yesod.Core.Unsafe    as Unsafe

-- | The foundation datatype for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.
data App = App
    { appSettings    :: AppSettings
    , appConnPool    :: ConnectionPool -- ^ Database connection pool.
    , appHttpManager :: Manager
    , appLogger      :: Logger
    }

-- This is where we define all of the routes in our application. For a full
-- explanation of the syntax, please see:
-- http://www.yesodweb.com/book/routing-and-handlers
--
-- Note that this is really half the story; in Application.hs, mkYesodDispatch
-- generates the rest of the code. Please see the following documentation
-- for an explanation for this split:
-- http://www.yesodweb.com/book/scaffolding-and-the-site-template#scaffolding-and-the-site-template_foundation_and_application_modules
--
-- This function also generates the following type synonyms:
-- type Handler = HandlerFor App
-- type Widget = WidgetFor App ()
mkYesodData "App" $(parseRoutesFile "config/routes")

-- | A convenient synonym for database access functions.
type DB a = forall (m :: * -> *).
    (MonadIO m, Functor m) => ReaderT SqlBackend m a

-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod App where
    -- Controls the base of generated URLs. For more information on modifying,
    -- see: https://github.com/yesodweb/yesod/wiki/Overriding-approot
    approot = ApprootRequest $ \app req ->
        fromMaybe (getApprootText guessApproot app req)
          (appRoot $ appSettings app)

    -- Store session data on the client in encrypted cookies,
    -- default session idle timeout is 120 minutes
    makeSessionBackend _ = Just <$> defaultClientSessionBackend
        120    -- timeout in minutes
        "config/client_session_key.aes"

    -- Yesod Middleware allows you to run code before and after each handler function.
    -- The defaultYesodMiddleware adds the response header "Vary: Accept, Accept-Language" and performs authorization checks.
    -- Some users may also want to add the defaultCsrfMiddleware, which:
    --   a) Sets a cookie with a CSRF token in it.
    --   b) Validates that incoming write requests include that token in either a header or POST parameter.
    -- To add it, chain it together with the defaultMiddleware: yesodMiddleware = defaultYesodMiddleware . defaultCsrfMiddleware
    -- For details, see the CSRF documentation in the Yesod.Core.Handler module of the yesod-core package.
    yesodMiddleware = defaultYesodMiddleware

    -- Routes requiring authentication.
    isAuthorized ArticlesR True            = isAuthenticated
    isAuthorized ArticlesFeedR _           = isAuthenticated
    isAuthorized (ArticleCommentsR _) True = isAuthenticated
    isAuthorized (ArticleCommentR _ _) _   = isAuthenticated
    isAuthorized (ArticleFavoriteR _) _    = isAuthenticated
    isAuthorized (FollowR _) _             = isAuthenticated
    isAuthorized UserR _                   = isAuthenticated

    -- Routes not requiring authentication.
    isAuthorized ArticlesR _               = return Authorized
    isAuthorized (ArticleR _) _            = return Authorized
    isAuthorized (ArticleCommentsR _) _    = return Authorized
    isAuthorized (ProfilesR _) _           = return Authorized
    isAuthorized UsersRegisterR _          = return Authorized
    isAuthorized UsersLoginR _             = return Authorized
    isAuthorized TagsR _                   = return Authorized

    -- What messages should be logged. The following includes all messages when
    -- in development, and warnings and errors in production.
    shouldLogIO app _source level =
        return $
        appShouldLogAll (appSettings app)
            || level == LevelWarn
            || level == LevelError

    makeLogger = return . appLogger

-- How to run database actions.
instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend
    runDB action = do
        master <- getYesod
        runSqlPool action $ appConnPool master

instance YesodPersistRunner App where
    getDBRunner = defaultGetDBRunner appConnPool

instance YesodAuth App where
    type AuthId App = UserId

    -- Where to send a user after successful login
    loginDest _ = ArticlesR
    -- Where to send a user after logout
    logoutDest _ = ArticlesR
    -- Override the above two destinations when a Referer: header is present
    redirectToReferer _ = True

    authPlugins _ = []

    authHttpManager = error "Doesn't need an HTTP manager"

    authenticate _ =
      maybe (UserError AuthMsg.InvalidLogin) Authenticated <$> maybeAuthId

    maybeAuthId = do
      mToken <- JWT.lookupToken
      liftHandler $ maybe (return Nothing) tokenToUserId mToken

-- | Access function to determine if a user is logged in.
isAuthenticated :: Handler AuthResult
isAuthenticated = do
    muid <- maybeAuthId
    return $ case muid of
        Nothing -> Unauthorized "You must login to access this page"
        Just _  -> Authorized

instance YesodAuthPersist App

-- This instance is required to use forms. You can modify renderMessage to
-- achieve customized and internationalized form validation messages.
instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

-- Useful when writing code that is re-usable outside of the Handler context.
-- An example is background jobs that send email.
-- This can also be useful for writing code that works across multiple Yesod applications.
instance HasHttpManager App where
    getHttpManager = appHttpManager

unsafeHandler :: App -> Handler a -> IO a
unsafeHandler = Unsafe.fakeHandlerGetLogger appLogger

-- Note: Some functionality previously present in the scaffolding has been
-- moved to documentation in the Wiki. Following are some hopefully helpful
-- links:
--
-- https://github.com/yesodweb/yesod/wiki/Sending-email
-- https://github.com/yesodweb/yesod/wiki/Serve-static-files-from-a-separate-domain
-- https://github.com/yesodweb/yesod/wiki/i18n-messages-in-the-scaffolding

userIdToToken :: UserId -> HandlerFor App Text
userIdToToken userId = do
  jwtSecret <- getJwtSecret
  return $ JWT.jsonToToken jwtSecret $ toJSON userId

tokenToUserId :: Text -> Handler (Maybe UserId)
tokenToUserId token = do
  jwtSecret <- getJwtSecret
  let mUserId = fromJSON <$> JWT.tokenToJson jwtSecret token
  case mUserId of
    Just (Success userId) -> return $ Just userId
    _                     -> return Nothing

getJwtSecret :: HandlerFor App Text
getJwtSecret =
  getsYesod $ appJwtSecret . appSettings
