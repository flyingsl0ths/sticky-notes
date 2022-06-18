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

import           Network.HTTP.Types             ( badRequest400
                                                , ok200
                                                )

import           Database.Persist.Sql           ( SqlBackend
                                                , SqlPersistT
                                                , runSqlConn
                                                )

import           Control.Monad.Logger           ( LoggingT
                                                , runStdoutLoggingT
                                                )

import           Web.Spock                      ( ActionCtxT
                                                , HasSpock(SpockConn, runQuery)
                                                , SpockM
                                                , WebStateM
                                                , get
                                                , getContext
                                                , header
                                                , json
                                                , prehook
                                                , root
                                                , setHeader
                                                , setStatus
                                                )

type AppDb = SqlBackend
type AppSession = ()
type AppState = ()
type App = SpockM AppDb AppSession AppState ()
type AppAction a = ActionCtxT () (WebStateM AppDb AppSession AppState) a
type AppRoute = App

data ServerContext = ServerContext
  { domain    :: Maybe String
  , debugMode :: Bool
  }
  deriving Show

runSQL
  :: (HasSpock m, SpockConn m ~ SqlBackend)
  => SqlPersistT (LoggingT IO) a
  -> m a
runSQL action = runQuery $ \conn -> runStdoutLoggingT $ runSqlConn action conn

adminPasswordHash = "" :: Text

routes :: ServerContext -> App
routes ctx = do
  let origin = if debugMode ctx then "*" else maybe "" pack (domain ctx)

  withCorsEnabled origin $ homeRoute ctx

homeRoute :: ServerContext -> AppRoute
homeRoute ctx = do
  get root $ do
    setStatus ok200
    json $ object ["message" .= String "Hello World"]

badRequest :: AppAction a
badRequest = do
  setStatus badRequest400
  json $ object []

withCorsEnabled :: Text -> AppRoute -> AppRoute
withCorsEnabled origin = prehook $ corsHeader origin

corsHeader origin = do
  ctx <- getContext
  setHeader "Access-Control-Allow-Origin" origin
  return ctx

