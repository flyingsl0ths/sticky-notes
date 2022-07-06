{-# LANGUAGE OverloadedStrings #-}

module StringUtils
  ( containsFileExtension
  , containsBadWord
  , containsUrl
  ) where

import           Data.Text                      ( Text
                                                , pack
                                                )
import           ListUtils                      ( indexOf )
import           Text.Regex.Pcre2               ( Option(Caseless)
                                                , matchesOpt
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

containsUrl :: Text -> Bool
containsUrl = matchesOpt
  Caseless
  "(:?http(:?s)?://)?(:?(:?(:?\\.?\\d+)+|localhost):\\d+|(:?www)?\\.[\\w\\d]+)(:?/[\\w\\d]+)*/?"

containsBadWord :: Text -> Bool
containsBadWord word = any (matcher word) badWordPatterns
 where
  matcher = flip $ matchesOpt Caseless
  badWordPatterns =
    [ "f+[\\w\\d]+c+k+(:?[e3]+r+|[i1]+n+[g9]+)?"
    , "b+[1i]+t+c+h+(:?[i1]+n+[g9]+|[e3]+[s5\\$]+|y+)?"
    , "c+u+(:?n+t+|m+)"
    , "[s5\\$]+[e3]+x+"
    , "p+[uo0]+[s5\\$]+[s5\\$]+y+"
    , "(:?p+|p+r+)[o0]+(:?r+n+|n+)"
    , "p+[e3]+n+[i1]+[s5\\$]+"
    , "k+[i1]+l+"
    , "[s5\\$]+h+[i1]+t+"
    , "[s5\\$]+u+[i1]+c+[i1]+d+[e3]+"
    , "[s5\\$]+[1l]+u+t+y*"
    , "c+[l1]+[i1]*t+"
    , "(:?(:?b|[g9])a+n+[g9])+"
    , "[g9]+a+y+"
    , "h+[o0]+m+[o0]+"
    , "f+u+r+y"
    , "j+(:?a+c+|e+r+)+k+(:?[i1]+n+[g9]+)?[o0]+f+"
    , "j+[i1]+z+"
    , "p+h+[i1]+l+[e3]+"
    , "c+u+c+k+(:?[e3]+r+)?"
    , "d+a+d+y"
    , "m+[o0]+m+y+"
    , "m+a+[s5\\$]+t+u+r+b+(:?a+(:?t+[e3]+|i+t+)|8+)"
    , "[o0]+r+[g9]+(:?y+|a+[s5\\$]+m+)"
    , "c+[o0]+n+d+[o0]+m+[s5\\$]*"
    , "(:?n+|c+h+)[i1]+[g9]+(:?a+|e*r*)"
    , "b+[e3]+a+n+[e*3*]+r+"
    , "c+[o0]+c+k*"
    , "[s5\\$]+h+[i1]+t+"
    , "[g9]+[o0]+r+[e3]+"
    ]
