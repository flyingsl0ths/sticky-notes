module ServerConfig
  ( ServerConfig
  , port
  , dbName
  , debugMode
  , dbPoolSize
  , logFile
  , domainName
  , getServerConfig
  ) where

import           FileUtils                      ( containsFileExtension )

import qualified Data.Maybe                    as DM
                                                ( fromMaybe )

import           System.Environment             ( lookupEnv )

import           Text.Read                      ( readMaybe )

defaultDBName :: String
defaultDBName = "notes.db"

data ServerConfig = ServerConfig
  { port       :: Int
  , dbName     :: String
  , debugMode  :: Bool
  , dbPoolSize :: Int
  , logFile    :: String
  , domainName :: Maybe String
  }
  deriving Show

getServerConfig :: IO ServerConfig
getServerConfig = do
  port'       <- getEnvVarAsNumber "ST_SVR_PORT" 8080
  debug       <- getEnvVarAsNumber "ST_SVR_DEBUG" 1
  dbName'     <- getDBName
  dbPoolSize' <- getEnvVarAsNumber "ST_SVR_POOL_SIZE" 5
  logFile'    <- getEnvVarOrDefault "ST_SVR_LOG" "server.log"
  domainName' <- lookupEnv "ST_DOMAIN_NAME"
  return $ ServerConfig { port       = port'
                        , dbName     = dbName'
                        , debugMode  = debug == 1
                        , dbPoolSize = dbPoolSize'
                        , logFile    = logFile'
                        , domainName = domainName'
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

getEnvVarOrDefault :: String -> String -> IO String
getEnvVarOrDefault envVar defaultValue = do
  value <- lookupEnv envVar
  return $ DM.fromMaybe defaultValue value
