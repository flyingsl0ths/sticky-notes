module ServerConfig
  ( ServerConfig
  , port
  , dbName
  , debugMode
  , dbPoolSize
  , domainName
  , adminPsswdHash
  , getServerConfig
  ) where

import qualified Data.Maybe                    as DM
                                                ( fromMaybe )
import           StringUtils                    ( containsFileExtension )
import           System.Environment             ( lookupEnv )
import           Text.Read                      ( readMaybe )

defaultDBName :: String
defaultDBName = "notes.db"

data ServerConfig = ServerConfig
  { port           :: Int
  , dbName         :: String
  , debugMode      :: Bool
  , dbPoolSize     :: Int
  , domainName     :: Maybe String
  , adminPsswdHash :: String
  }
  deriving Show

getServerConfig :: IO ServerConfig
getServerConfig = do
  port'             <- getEnvVarAsNumber "ST_SVR_PORT" 5000
  debug             <- getEnvVarAsNumber "ST_SVR_DEBUG" 1
  dbName'           <- getDBName
  dbPoolSize'       <- getEnvVarAsNumber "ST_SVR_POOL_SIZE" 5
  domainName'       <- lookupEnv "ST_DOMAIN_NAME"
  adminPasswordHash <- lookupEnv "ST_ADMIN_PASSWORD_HASH"
  return $ ServerConfig { port           = port'
                        , dbName         = dbName'
                        , debugMode      = debug == 1
                        , dbPoolSize     = dbPoolSize'
                        , domainName     = domainName'
                        , adminPsswdHash = DM.fromMaybe "" adminPasswordHash
                        }

getEnvVarAsNumber :: String -> Int -> IO Int
getEnvVarAsNumber envVar defaultValue = do
  value <- lookupEnv envVar
  return $ maybe defaultValue parseOrDefault value
  where parseOrDefault str = DM.fromMaybe defaultValue (readMaybe str)

getDBName :: IO String
getDBName = do
  value <- lookupEnv "ST_DB"
  return $ maybe defaultDBName validDBFileOrDefault value
 where
  validDBFileOrDefault file | containsFileExtension file "db" = file
                            | otherwise                       = defaultDBName
