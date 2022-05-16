module FileUtils
  ( containsFileExtension
  ) where

import           ListUtils                      ( indexOf )

containsFileExtension :: String -> String -> Bool
containsFileExtension fileName extension =
  let theDot = indexOf '.' fileName
  in  (theDot /= -1) && matchesExtension theDot
 where
  matchesExtension theDot
    | theDot == length fileName
    = False
    | otherwise
    = let theDot'         = theDot + 1
          (_, extension') = splitAt theDot' fileName
      in  extension == extension'
