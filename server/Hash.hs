import qualified Crypto.Hash as CH (SHA256 (..), hashWith)
import qualified Data.ByteString.UTF8 as DBU8 (fromString)
import qualified System.Environment as SE (getArgs)

main :: IO ()
main =
    do
        argv <- SE.getArgs

        let arg =
                if null argv
                    then "Lorem ipsum dolor sit amet"
                    else head argv

        print $ hash $ arg
  where
    hash ms = show $ CH.hashWith CH.SHA256 $ DBU8.fromString ms
