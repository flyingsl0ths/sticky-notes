{-# LANGUAGE OverloadedStrings #-}

module Routes
  ( routes
  , App
  , AppDb
  , AppSession
  , AppState
  , AppRoute
  ) where

import           Data.Aeson                     ( KeyValue((.=))
                                                , Value(String)
                                                , object
                                                )

import           Data.Text                      ( Text )

import           Network.HTTP.Types             ( ok200 )

import           Web.Spock                      ( SpockM
                                                , get
                                                , getContext
                                                , json
                                                , prehook
                                                , root
                                                , setHeader
                                                , setStatus
                                                )

type AppDb = ()
type AppSession = ()
type AppState = ()
type App = SpockM AppDb AppSession AppState ()
type AppRoute = App

-- ! Change once deployed
domain :: Text
domain = "..."

routes :: Bool -> App
routes debug = do
  let origin = if debug then "*" else domain
  withCorsEnabled origin homeRoute

homeRoute :: AppRoute
homeRoute = do
  get root $ do
    setStatus ok200
    json $ object ["message" .= String "Hello World"]

withCorsEnabled :: Text -> AppRoute -> AppRoute
withCorsEnabled origin = prehook $ corsHeader origin

corsHeader origin = do
  ctx <- getContext
  setHeader "Access-Control-Allow-Origin" origin
  return ctx
