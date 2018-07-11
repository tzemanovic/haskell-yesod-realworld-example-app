module TestImport
    ( module TestImport
    , module X
    ) where

import           Application                   (makeFoundation, makeLogWare)
import           ClassyPrelude                 as X hiding (Handler, delete,
                                                     deleteBy)
import           Data.Aeson                    (FromJSON, Result (..), decode,
                                                fromJSON)
import           Database.Persist.Extended     as X hiding (get)
import           Database.Persist.Sql          (SqlPersistM, connEscapeName,
                                                rawExecute, rawSql,
                                                runSqlPersistMPool, unSingle)
import           Foundation                    as X
import           Model                         as X
import           Test.Hspec                    as X
import           Yesod.Auth                    as X
import           Yesod.Core.Unsafe             (fakeHandlerGetLogger)
import           Yesod.Default.Config2         (loadYamlSettings, useEnv)
import           Yesod.Test                    as X

-- Wiping the database
import           Control.Monad.Logger          (runLoggingT)
import qualified Data.ByteString.Lazy.Char8    as C
import           Database.Persist.Sqlite       (createSqlitePoolFromInfo,
                                                fkEnabled,
                                                mkSqliteConnectionInfo,
                                                sqlDatabase)
import           Lens.Micro                    (set)
import           Network.HTTP.Types.Header     as X
import           Network.Wai.Test              (SResponse (..))
import           Settings                      (appDatabaseConf)
import           System.Environment            (setEnv)
import           Test.HUnit                    as X (assertFailure)
import           Yesod.Auth.Util.PasswordStore (makePassword)
import           Yesod.Core                    (messageLoggerSource)

runDB :: SqlPersistM a -> YesodExample App a
runDB query = do
    pool <- fmap appConnPool getTestYesod
    liftIO $ runSqlPersistMPool query pool

runHandler :: Handler a -> YesodExample App a
runHandler handler = do
    app <- getTestYesod
    fakeHandlerGetLogger appLogger app handler

-- | Spec runner that sets up a test environment with DB.
withApp :: SpecWith (TestApp App) -> Spec
withApp = before $ do
    setEnv "JWT_SECRET" "test"
    settings <- loadYamlSettings
        ["config/test-settings.yml", "config/settings.yml"]
        []
        useEnv
    foundation <- makeFoundation settings
    wipeDB foundation
    logWare <- liftIO $ makeLogWare foundation
    return (foundation, logWare)

-- | Insert a user into the DB.
insertUser :: Text -> Email -> Text -> YesodExample App (Key User)
insertUser username email password = do
  pwdHash <- liftIO $ makePassword (encodeUtf8 password) 14
  now <- liftIO getCurrentTime
  runDB $ insert User
        { userEmail = email
        , userUsername = username
        , userPassword = decodeUtf8 pwdHash
        , userBio = ""
        , userImage = ""
        , userCreatedAt = now
        , userUpdatedAt = now
        }

-- | Get response from JSON body.
getJsonResponse :: FromJSON a => YesodExample App a
getJsonResponse =
  withResponse $ \SResponse {..} ->
    case fromJSON <$> decode simpleBody of
      Just (Success a) -> return a
      _ -> lift $ assertFailure $ "cannot decode JSON: " ++ C.unpack simpleBody

-- | Build a request that gets a JWT token for a given username and uses it
-- to set the request's authentication header.
authenticatedRequest :: Text -> RequestBuilder App () -> YesodExample App ()
authenticatedRequest username reqBuilder = do
  token <- runHandler $ usernameToJwtToken username
  request $ do
    addRequestHeader (hAuthorization, "token " ++ encodeUtf8 token)
    reqBuilder

-- | This function will truncate all of the tables in your database.
-- 'withApp' calls it before each test, creating a clean environment for each
-- spec to run in.
wipeDB :: App -> IO ()
wipeDB app = do
    -- In order to wipe the database, we need to use a connection which has
    -- foreign key checks disabled.  Foreign key checks are enabled or disabled
    -- per connection, so this won't effect queries outside this function.
    --
    -- Aside: foreign key checks are enabled by persistent-sqlite, as of
    -- version 2.6.2, unless they are explicitly disabled in the
    -- SqliteConnectionInfo.

    let logFunc = messageLoggerSource app (appLogger app)

    let dbName = sqlDatabase $ appDatabaseConf $ appSettings app
        connInfo = set fkEnabled False $ mkSqliteConnectionInfo dbName

    pool <- runLoggingT (createSqlitePoolFromInfo connInfo 1) logFunc

    flip runSqlPersistMPool pool $ do
        tables <- getTables
        sqlBackend <- ask
        let queries = map (\t -> "DELETE FROM " ++ connEscapeName sqlBackend (DBName t)) tables
        forM_ queries (\q -> rawExecute q [])

getTables :: DB [Text]
getTables = do
    tables <- rawSql "SELECT name FROM sqlite_master WHERE type = 'table';" []
    return (fmap unSingle tables)
