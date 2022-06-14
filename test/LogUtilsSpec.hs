import           LogUtils

import           Test.Hspec                     ( Spec
                                                , describe
                                                , hspec
                                                , it
                                                , shouldBe
                                                )

import           System.IO                      ( Handle
                                                , IOMode(ReadMode)
                                                , hGetLine
                                                , withFile
                                                )

import           System.Directory               ( removeFile )

import           Control.Exception              ( catch
                                                , throwIO
                                                )

import           System.IO.Error                ( isDoesNotExistError )

import           Control.Monad.Logger           ( LoggingT
                                                , logInfoN
                                                , runFileLoggingT
                                                )

import           Data.Text                      ( pack )

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "LogUtils.makeRequestLogger" $ do

  it "Logs: \"NO CONTEXT GIVEN\" when context contains empty fields" $ do
    let ctx = RequestRecordC { requestType = Get
                             , route       = ""
                             , username    = ""
                             , time        = ""
                             }

    let logFile = "app.log"

    let logger =
          (  makeRequestLogger ctx logInfoN
          >> logInfoN (pack "SELECT * FROM SOME_TABLE")
          ) :: LoggingT IO ()

    runFileLoggingT logFile logger

    let inspectLog = \hdl -> do
          ctx' <- hGetLine hdl
          return (ctx' == "[Error] NO CONTEXT GIVEN")

    containsNoContext <- withReadOnlyFile logFile inspectLog

    containsNoContext `shouldBe` True


withReadOnlyFile :: FilePath -> (Handle -> IO a) -> IO a
withReadOnlyFile path f = withFile
  path
  ReadMode
  (\hdl -> do
    let result = f hdl
    removeIfExists path
    result
  )

removeIfExists :: FilePath -> IO ()
removeIfExists fileName = removeFile fileName `catch` handleExists
 where
  handleExists e | isDoesNotExistError e = return ()
                 | otherwise             = throwIO e
