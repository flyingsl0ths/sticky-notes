module Main where

import           ServerConfig                   ( ServerConfig
                                                  ( dbName
                                                  , dbPoolSize
                                                  , debugMode
                                                  , domainName
                                                  , logFile
                                                  , port
                                                  )
                                                , getServerConfig
                                                )

import           Routes                         ( ServerContext
                                                  ( ServerContextC
                                                  , domain
                                                  )
                                                , routes
                                                )

import           Web.Spock                      ( runSpockNoBanner
                                                , spock
                                                )

import           Web.Spock.Config               ( PoolOrConn(PCPool)
                                                , defaultSpockCfg
                                                )

import           Control.Monad.Logger           ( logInfoN
                                                , runFileLoggingT
                                                , runNoLoggingT
                                                )

import           Database.Persist.Sqlite        ( createSqlitePool
                                                , runMigration
                                                , runSqlPool
                                                )

import           Data.Text                      ( pack )

import           Note                           ( migrateAll )


main :: IO ()
main = do
  (serverConfig, app') <- setup

  runSpockNoBanner (port serverConfig) app'

setup = do
  serverConfig <- getServerConfig

  let logFile' = logFile serverConfig

  pool <- runFileLoggingT
    logFile'
    (do
      logInfoN (pack $ show serverConfig)

      pool <- do
        logInfoN (pack "Initializing database")
        createSqlitePool (pack $ dbName serverConfig) (dbPoolSize serverConfig)

      logInfoN $ pack "Running migrations"
      runSqlPool
        (do
          runMigration migrateAll
        )
        pool

      return pool
    )

  spockCfg <- defaultSpockCfg () (PCPool pool) ()

  let ctx = ServerContextC (domainName serverConfig)
                           (debugMode serverConfig)
                           logFile'

  return (serverConfig, spock spockCfg $ routes ctx)

