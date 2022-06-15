{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}

module Routes
  ( routes
  , App
  , AppDb
  , AppSession
  , AppState
  , AppRoute
  , ServerContext(..)
  ) where

import           Data.Aeson                     ( KeyValue((.=))
                                                , Value(String)
                                                , defaultOptions
                                                , object
                                                )

import           Data.Text                      ( Text
                                                , pack
                                                )

import           Network.HTTP.Types             ( ok200 )

import           Database.Persist.Sql           ( SqlBackend
                                                , SqlPersistT
                                                , runSqlConn
                                                )

import           Web.Spock                      ( HasSpock(SpockConn, runQuery)
                                                , SpockM
                                                , get
                                                , getContext
                                                , json
                                                , prehook
                                                , root
                                                , setHeader
                                                , setStatus
                                                )

import           Control.Monad.Logger           ( LoggingT
                                                , runFileLoggingT
                                                , runStdoutLoggingT
                                                )

data ServerContext = ServerContextC
  { domain    :: Maybe String
  , debugMode :: Bool
  , logFile   :: String
  }
  deriving Show

runSQL
  :: (HasSpock m, SpockConn m ~ SqlBackend)
  => FilePath
  -> LoggingT IO a
  -> SqlPersistT (LoggingT IO) a
  -> m a
runSQL logFile logger action =
  runQuery $ \conn -> runFileLoggingT logFile (logger >> runSqlConn action conn)

type AppDb = SqlBackend
type AppSession = ()
type AppState = ()
type App = SpockM AppDb AppSession AppState ()
type AppRoute = App

routes :: ServerContext -> App
routes ctx = do
  let origin = if debugMode ctx then "*" else maybe "" pack (domain ctx)

  withCorsEnabled origin $ homeRoute ctx

homeRoute :: ServerContext -> AppRoute
homeRoute ctx = do
  get root $ do
    setStatus ok200
    json $ object ["message" .= String "Hello World"]

withCorsEnabled :: Text -> AppRoute -> AppRoute
withCorsEnabled origin = prehook $ corsHeader origin

corsHeader origin = do
  ctx <- getContext
  setHeader "Access-Control-Allow-Origin" origin
  return ctx
