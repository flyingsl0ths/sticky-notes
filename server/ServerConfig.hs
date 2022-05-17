module ServerConfig
  ( ServerConfig
  , port
  , dbName
  , debugMode
  , getServerConfig
  ) where

import           FileUtils                      ( containsFileExtension )

import qualified Data.Maybe                    as DM
                                                ( fromMaybe )

import           System.Environment             ( lookupEnv )

import           Data.List                      ( foldl' )
import           Text.Read                      ( readMaybe )

defaultPortNo :: Int
defaultPortNo = 8080

defaultDBName :: String
defaultDBName = "notes.db"

defaultDebugMode :: Int
defaultDebugMode = 1

portEnvVar :: String
portEnvVar = "ST_SVR_PORT"

dbNameEnvVar :: String
dbNameEnvVar = "ST_DB"

debugModeEnvVar :: String
debugModeEnvVar = "ST_SVR_DEBUG"

data ServerConfig = ServerConfig
  { port      :: Int
  , dbName    :: String
  , debugMode :: Bool
  }
  deriving Show

getServerConfig :: IO ServerConfig
getServerConfig = do
  port'   <- getEnvVarAsNumber portEnvVar defaultPortNo
  debug   <- getEnvVarAsNumber debugModeEnvVar defaultDebugMode
  dbName' <- getDBName
  return
    $ ServerConfig { port = port', dbName = dbName', debugMode = debug == 1 }

getEnvVarAsNumber :: String -> Int -> IO Int
getEnvVarAsNumber envVar defaultValue = do
  value <- lookupEnv envVar
  return $ maybe defaultValue parseOrDefault value
  where parseOrDefault str = DM.fromMaybe defaultValue (readMaybe str)

getDBName :: IO String
getDBName = do
  value <- lookupEnv dbNameEnvVar
  return $ maybe defaultDBName validDBFileOrDefault value
 where
  validDBFileOrDefault file | containsFileExtension file "db" = file
                            | otherwise                       = defaultDBName