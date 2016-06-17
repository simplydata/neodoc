module Data.String.Ext (
    (^=), (^/=), (^=^)
  , upperCaseEq
  , notUpperCaseEq
  , endsWith
  , startsWith
  , stripAngles
  , posArgsEq
  ) where

import Prelude
import Data.Maybe (maybe, fromMaybe)
import Data.String as Str
import Data.Function (on)

upperCaseEq :: String -> String -> Boolean
upperCaseEq = eq `on` Str.toUpper
infixl 9 upperCaseEq as ^=

notUpperCaseEq :: String -> String -> Boolean
notUpperCaseEq a b = not (a ^= b)
infixl 9 notUpperCaseEq as ^/=

startsWith :: String -> String -> Boolean
startsWith needle haystack = maybe false id do
  ix <- Str.indexOf needle haystack
  pure $ ix == 0

endsWith :: String -> String -> Boolean
endsWith needle haystack = maybe false id do
  ix <- Str.lastIndexOf needle haystack
  pure $ ix == (Str.length haystack - Str.length needle)

stripAngles :: String -> String
stripAngles = stripPrefix <<< stripSuffix
  where
  stripPrefix s = fromMaybe s (Str.stripPrefix "<" s)
  stripSuffix s = fromMaybe s (Str.stripSuffix ">" s)

posArgsEq :: String -> String -> Boolean
posArgsEq = eq `on` (Str.toUpper <<< stripAngles)
infixl 9 posArgsEq as ^=^
