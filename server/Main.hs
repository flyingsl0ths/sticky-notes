module Main where

import           Control.Monad.Logger           ( runNoLoggingT )
import           Data.Text                      ( pack )
import           Database.Persist.Sqlite        ( createSqlitePool
                                                , runMigration
                                                , runSqlPool
                                                )
import           Note                           ( migrateAll )
import           Routes                         ( ServerContext
                                                  ( ServerContext
                                                  , domain
                                                  )
                                                , routes
                                                )
import           ServerConfig                   ( ServerConfig
                                                  ( adminPsswdHash
                                                  , dbName
                                                  , dbPoolSize
                                                  , debugMode
                                                  , domainName
                                                  , port
                                                  )
                                                , getServerConfig
                                                )
import           Web.Spock                      ( runSpockNoBanner
                                                , spock
                                                )
import           Web.Spock.Config               ( PoolOrConn(PCPool)
                                                , defaultSpockCfg
                                                )


main :: IO ()
main = do
  (portNumber, app') <- setup

  runSpockNoBanner portNumber app'

setup = do
  serverConfig <- getServerConfig

  pool         <- runNoLoggingT
    $ createSqlitePool (pack $ dbName serverConfig) (dbPoolSize serverConfig)

  runSqlPool
    (do
      runMigration migrateAll
    )
    pool

  spockCfg <- defaultSpockCfg () (PCPool pool) ()

  let serverCtx = ServerContext (domainName serverConfig)
                                (debugMode serverConfig)
                                (pack $ adminPsswdHash serverConfig)

  return (port serverConfig, spock spockCfg $ routes serverCtx)

