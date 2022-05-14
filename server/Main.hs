module Main
  ( main
  , app
  ) where

import           Config                         ( ServerConfig(debugMode, port)
                                                , getServerConfig
                                                )

import           Routes                         ( routes )

import           Web.Spock                      ( runSpock
                                                , runSpockNoBanner
                                                , spock
                                                )
import           Web.Spock.Config               ( PoolOrConn(PCNoDatabase)
                                                , defaultSpockCfg
                                                )


main :: IO ()
main = do
  (serverConfig, app') <- app
  print serverConfig
  runSpockNoBanner (port serverConfig) app'

app = do
  serverConfig <- getServerConfig
  spockCfg     <- defaultSpockCfg () PCNoDatabase ()
  let debug = debugMode serverConfig
  return (serverConfig, spock spockCfg $ routes debug)
