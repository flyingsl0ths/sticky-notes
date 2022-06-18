module Main where

import           ServerConfig                   ( ServerConfig
                                                  ( dbName
                                                  , dbPoolSize
                                                  , debugMode
                                                  , domainName
                                                  , port
                                                  )
                                                , getServerConfig
                                                )

import           Routes                         ( ServerContext
                                                  ( ServerContext
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

import           Control.Monad.Logger           ( runNoLoggingT )

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

  pool         <- do
    runNoLoggingT $ createSqlitePool (pack $ dbName serverConfig)
                                     (dbPoolSize serverConfig)

  runSqlPool
    (do
      runMigration migrateAll
    )
    pool

  spockCfg <- defaultSpockCfg () (PCPool pool) ()

  let ctx = ServerContext (domainName serverConfig) (debugMode serverConfig)

  return (serverConfig, spock spockCfg $ routes ctx)

