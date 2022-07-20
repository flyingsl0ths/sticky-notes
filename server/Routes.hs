{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Routes (
    routes,
    App,
    AppDb,
    AppSession,
    AppState,
    AppRoute,
    ServerContext (..),
) where

import Control.Monad (
    unless,
    when,
 )

import Control.Monad.Cont (MonadIO)

import Control.Monad.Logger (
    LoggingT,
    runStdoutLoggingT,
 )

import Data.Aeson (
    KeyValue ((.=)),
    Value (Array, Bool, String),
    defaultOptions,
    object,
 )

import Data.Aeson.Types (
    Key,
    Pair,
 )

import Data.Text (
    Text,
    empty,
    pack,
    unpack,
 )

import qualified Database.Persist as Pst

import Database.Persist.Sql (
    SelectOpt (Asc),
    SqlBackend,
    SqlPersistT,
    insert,
    rawExecute,
    runSqlConn,
    selectList,
    (==.),
 )

import Network.HTTP.Types (
    badRequest400,
    ok200,
 )

import Note (
    EntityField (
        NoteAuthor,
        NoteId,
        NoteText,
        NoteTitle
    ),
    Note (..),
 )

import StringUtils (
    containsBadWord,
    containsUrl,
 )

import Network.Wai.Middleware.Static (addBase, staticPolicy)
import Web.Spock (
    ActionCtxT,
    HasSpock (SpockConn, runQuery),
    SpockM,
    WebStateM,
    delete,
    file,
    get,
    getContext,
    header,
    json,
    jsonBody',
    middleware,
    post,
    prehook,
    root,
    setHeader,
    setStatus,
    text,
 )

type AppDb = SqlBackend

type AppSession = ()

type AppState = ()

type App = SpockM AppDb AppSession AppState ()

type AppAction a = ActionCtxT () (WebStateM AppDb AppSession AppState) a

type AppRoute = App

data ServerContext = ServerContext
    { domain :: Maybe String
    , debugMode :: Bool
    , adminPasswordHash :: Text
    }
    deriving (Show)

runSQL ::
    (HasSpock m, SpockConn m ~ SqlBackend) =>
    SqlPersistT (LoggingT IO) a ->
    m a
runSQL action = runQuery $ \conn -> runStdoutLoggingT $ runSqlConn action conn

routes :: ServerContext -> App
routes ctx = do
    let origin = if debugMode ctx then "*" else maybe "" pack (domain ctx)

    withCorsEnabled origin $ do
        homeRoute
        allNotes
        addNote
        deleteAllNotes $ adminPasswordHash ctx
        helloRoute

helloRoute :: AppRoute
helloRoute = do
    get "hello" $ do
        setStatus ok200
        json $ object ["message" .= String "Hello World!"]

homeRoute :: AppRoute
homeRoute = do
    middleware $ staticPolicy $ addBase "client"
    get root $ do
        setStatus ok200
        file "text/html" "index.html"

allNotes :: AppRoute
allNotes = get "notes" $ do
    notes <- runSQL $ selectList [] [Asc NoteId]
    setStatus ok200
    json notes

addNote :: AppRoute
addNote = post "note" $ do
    note <- jsonBody' :: AppAction Note
    onNoteAdded note
  where
    hasAuthorOrDefault "" = "Anon."
    hasAuthorOrDefault author = author

    inspectNote author title content =
        let fields =
                (
                    [ "noteAuthorNameInvalid"
                    , "noteTitleNameInvalid"
                    , "noteContentInvalid"
                    ]
                , [author, title, content]
                )
         in inspect fields (\txt -> containsBadWord txt || containsUrl txt)

    onCreatePost :: Note -> AppAction ()
    onCreatePost note = do
        runSQL $ insert note
        setStatus ok200
        json $ object ["status" .= String "New note created"]

    isDuplicateOrCreate newPost = do
        isDuplicate <- isDuplicatePost newPost

        if isDuplicate
            then badRequestJson ["status" .= String "Duplicate post"]
            else onCreatePost newPost

    onNoteAdded Note{noteTitle = title, noteText = content, noteAuthor = author, noteDate = date} =
        do
            inspectNote author title content

            let newNote =
                    Note
                        { noteTitle = title
                        , noteText = content
                        , noteAuthor = hasAuthorOrDefault author
                        , noteDate = date
                        }

            isDuplicateOrCreate newNote

deleteAllNotes :: Text -> AppRoute
deleteAllNotes adminPasswordHash = delete "notes" $ do
    password <- header "password"
    maybe (badRequest "PASSWORD REQUIRED") onDeleteAllPosts password
  where
    onDeleteAllPosts password
        | adminPasswordHash == empty =
            badRequest
                "ERROR NO CREDENTIALS SPECIFIED PLEASE CONTACT DEVELOPER"
        | password /= adminPasswordHash = badRequest "INVALID PASSWORD"
        | otherwise = do
            runSQL $ rawExecute "delete from note" []
            setStatus ok200

inspect :: ([Data.Aeson.Types.Key], [Text]) -> (Text -> Bool) -> AppAction ()
inspect (fields, values) p = do
    let results = map p values

    when (and results || False `elem` results) $
        badRequestJson $
            zipWith
                (\f r -> f .= Bool r)
                fields
                results

badRequestJson :: [Pair] -> AppAction a
badRequestJson ps = do
    setStatus badRequest400
    json $ object ps

isDuplicatePost :: Note -> AppAction Bool
isDuplicatePost Note{noteAuthor = author, noteTitle = title, noteText = content} =
    do
        duplicates <-
            runSQL $
                Pst.count
                    [NoteAuthor ==. author, NoteTitle ==. title, NoteText ==. content]
        return $ duplicates /= 0

badRequest :: Text -> AppAction a
badRequest msg = do
    setStatus badRequest400
    text msg

withCorsEnabled :: Text -> AppRoute -> AppRoute
withCorsEnabled origin = prehook $ corsHeader origin

corsHeader :: MonadIO m => Text -> ActionCtxT b m b
corsHeader origin = do
    ctx <- getContext
    setHeader "Access-Control-Allow-Origin" origin
    return ctx
