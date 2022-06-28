module FileUtils
  ( containsFileExtension
  , containsBadWords
  ) where

import           ListUtils                      ( indexOf )
import           Text.Regex                     ( matchRegex
                                                , mkRegexWithOpts
                                                )

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

containsBadWords :: String -> Bool
containsBadWords text =
  let
    badWordsPattern =
      "(f+[a-zA-Z0]+c+k+([e3]*r*|[i1]+n+[g9]+)|b+[1i]+t+c+h+([i1]+n+[g9]+|[e3]+[s5\\$]+)|c+u+(n+t+|m+)|"
        ++ "[s5\\$]+[e3]+x+|p+[uo0]+[s5\\$]+[s5\\$]+y+|(p+|p+r*)[o0]+r*n+|"
        ++ "p+[e3]+n+[i1]+[s5\\$]+|k+[i1]+l+|[s5\\$]+h+[i1]+t+|[s5\\$]+u+[i1]+c+[i1]+d+[e3]+|"
        ++ "c+[l1]+[i1]*t+|[g9]+a+n+[g9]+b+a+n+[g9]+|[g9]+a+y+|h+[o0]+m+[o0]+|f+u+r+y|"
        ++ "j+(a+c+|e+r+)+k+([i1]+n+[g9]+)?[o0]+f+|j+[i1]+z+|p+h+[i1]+l+[e3]+|c+u+(c|k)+[e3]*r*|d+a+d+y|m+[o0]+m+y+|"
        ++ "m+a+[s5\\$]+t+u+r+b+(a+t+[e3]+|8+|a+i+t+)|[o0]+r+[g9]+(y+|a+[s5\\$]+m+)|c+[o0]+n+d+[o0]+m+[s5\\$]*|"
        ++ "(n+|c+h+)[i1]+[g9]+(a+|e*r*)|b+[e3]+a+n+[e*3*]+r+|c+[o0]+c+k*|[s5\\$]+h+[i1]+t+|[g9]+[o0]+r+[e3]+)"
  in  withRegexPattern badWordsPattern text


containsURLs :: String -> Bool
containsURLs text =
  let
    urlsPattern
      = "((http(s)?://)?(www)?(((\\.?[0-9]+)+|localhost:[0-9]+)(/[a-zA-Z0-9]+)*|(\\.[a-zA-Z0-9]+)+)/?)"
      -- TODO: Fix regex pattern so inputs like "mr.foo" aren't treated as urls
  in  withRegexPattern urlsPattern text

withRegexPattern :: String -> String -> Bool
withRegexPattern pattern text =
  let badWordMatcher = mkRegexWithOpts pattern True False
  in  maybe False (not . null)
        $   filter (not . null)
        <$> matchRegex badWordMatcher text

