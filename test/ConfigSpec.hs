import           Config                         ( ServerConfig
                                                  ( dbName
                                                  , debugMode
                                                  , port
                                                  )
                                                , getServerConfig
                                                )

import           System.Environment             ( setEnv )

import           Test.Hspec                     ( Spec
                                                , describe
                                                , hspec
                                                , it
                                                , shouldBe
                                                )

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  defaultConfigSpec
  configSpec
  badConfigValuesSpec

portEnvVar :: String
portEnvVar = "ST_SVR_PORT"

dbNameEnvVar :: String
dbNameEnvVar = "ST_DB"

debugModeEnvVar :: String
debugModeEnvVar = "ST_SVR_DEBUG"

defaultConfigSpec :: Spec
defaultConfigSpec = describe "ServerConfig (default values)" $ do
  it "returns the default debug mode setting" $ do
    serverConfig <- getServerConfig
    debugMode serverConfig `shouldBe` True

  it "returns the default port number" $ do
    serverConfig <- getServerConfig
    port serverConfig `shouldBe` 8080

  it "returns the default db name" $ do
    serverConfig <- getServerConfig
    dbName serverConfig `shouldBe` "notes.db"

configSpec :: Spec
configSpec = describe "ServerConfig (expected values)" $ do
  it "returns the set debug mode setting" $ do
    setEnv debugModeEnvVar "1"
    serverConfig <- getServerConfig
    debugMode serverConfig `shouldBe` True


  it "returns the set port number" $ do
    setEnv portEnvVar "5050"
    serverConfig <- getServerConfig
    port serverConfig `shouldBe` 5050

  it "returns the set db name" $ do
    let db = "notes_prod.db"
    setEnv dbNameEnvVar db
    serverConfig <- getServerConfig
    dbName serverConfig `shouldBe` db

badConfigValuesSpec :: Spec
badConfigValuesSpec = describe "ServerConfig (bad values)" $ do
  it
      "returns the default debug mode setting when the parsed value is not a number"
    $ do
        setEnv debugModeEnvVar "false"
        serverConfig <- getServerConfig
        debugMode serverConfig `shouldBe` True

  it "returns the default port number when the parsed value is not a number"
    $ do
        setEnv portEnvVar "five zero five zero"
        serverConfig <- getServerConfig
        port serverConfig `shouldBe` 8080

  it
      "returns the default db name when the value does not contain a \".db\" extension"
    $ do
        let db = "notes"
        setEnv dbNameEnvVar db
        serverConfig <- getServerConfig
        dbName serverConfig `shouldBe` "notes.db"
