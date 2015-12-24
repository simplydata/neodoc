module Docopt.Spec.Parser.Scanner where

import Prelude
import Debug.Trace
import Control.MonadPlus (guard)
import Control.Alt ((<|>))
import Control.Apply ((*>), (<*))
import qualified Text.Parsing.Parser as P
import qualified Text.Parsing.Parser.Combinators as P
import qualified Text.Parsing.Parser.Pos as P
import qualified Text.Parsing.Parser.String as P
import Data.List (List(..), many, toList, fromList, (:), length, filter
                 , takeWhile, sort, head)
import qualified Data.Array as A
import Data.String (toLower, toCharArray, fromCharArray)
import qualified Data.String as Str
import Data.Maybe
import Data.Either
import Docopt.Spec.Parser.Base
import qualified Docopt.Spec.Parser.Lexer as Lexer
import qualified Docopt.Spec.Parser.Usage as Usage
import Text.Wrap (dedent)

type Docopt = { usage :: Section, options :: List Section }
type Section = String

scan :: String -> Either P.ParseError Docopt
scan = (flip P.runParser docoptScanner) <<< dedent

docoptScanner :: P.Parser String Docopt
docoptScanner = do

  P.manyTill
    P.anyChar
    ((void $ P.lookAhead sectionLabel) <|> P.eof)

  label <- sectionLabel
  guard ((toLower label) == "usage")

  -- "Fix" the section by replacing the original section header with whitespace
  -- to maintain proper offsets.
  fixColOffset <- (P.char '\n' *> pure 0)
              <|> (pure $ Str.length label + 1) -- + 1 for the colon
  usage <- fromCharArray <<< fromList <$> do
    P.manyTill
      P.anyChar
      ((void $ P.lookAhead sectionLabel) <|> P.eof)
  let fixedUsage = (fromCharArray $ A.replicate fixColOffset ' ') ++ usage

  options <- many do
    label <- sectionLabel
    guard $ endsWith ("options") (toLower label)
    fromCharArray <<< fromList <$> do
      P.manyTill
        P.anyChar
        ((void $ P.lookAhead sectionLabel) <|> P.eof)

  pure { usage: dedent fixedUsage, options: options }

  where
    sectionLabel :: P.Parser String String
    sectionLabel = do
      many space
      name <- fromCharArray <$> do
        A.many $ P.noneOf [ '\n', '\r', ':' ]
      P.char ':'
      pure name

    endsWith :: String -> String -> Boolean
    endsWith sub s = Str.drop (Str.length s - Str.length sub) s == sub