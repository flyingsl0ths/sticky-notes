module LogUtils
  ( Logger
  , RequestType(..)
  , RequestRecord(..)
  , makeRequestLogger
  , utcDate
  , fromUnixTime
  ) where

import           Control.Monad                  ( unless )

import           Control.Monad.Logger           ( LogLevel(LevelOther)
                                                , MonadLogger
                                                , logErrorN
                                                )

import           Data.Text                      ( Text
                                                , pack
                                                )

import           Data.Time.Clock.POSIX          ( POSIXTime
                                                , posixSecondsToUTCTime
                                                )

import           Data.Functor                   ( (<&>) )

import           Data.Time.Clock                ( getCurrentTime )

import           Data.Time.Format.ISO8601       ( iso8601Show )

fromUnixTime :: POSIXTime -> String
fromUnixTime = iso8601Show . posixSecondsToUTCTime

utcDate :: IO String
utcDate = getCurrentTime <&> show

type Logger m = (Text -> m ())

data RequestType =
   Get
  | Put   -- Available only for sake of completeness
  | Post
  | Delete

instance Show RequestType where
  show req = case req of
    Get    -> "GET"
    Put    -> "PUT"
    Post   -> "POST"
    Delete -> "DELETE"

data RequestRecord = RequestRecordC
  { requestType :: RequestType
  , route       :: String
  , username    :: String
  , time        :: String
  }
  deriving Show

makeRequestLogger :: MonadLogger m => RequestRecord -> Logger m -> m ()
makeRequestLogger ctx logger | containsRequiredFields = withLogger logger
                             | otherwise = logErrorN $ pack "NO CONTEXT GIVEN"
 where
  RequestRecordC { requestType = requestType', route = route', username = username', time = time' }
    = ctx

  containsRequiredFields =
    not $ null route' && null route' && null username' && null time'

  withLogger :: Logger m -> m ()
  withLogger logger =
    logger
      $  pack
      $  show requestType'
      ++ " /"
      ++ route'
      ++ " "
      ++ username'
      ++ " "
      ++ time'

