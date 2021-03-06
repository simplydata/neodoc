module Language.Docopt.ArgParser.Parser.Options (
  Options()
  ) where

-- | The options for generating a parser
type Options r = {
  optionsFirst :: Boolean
, stopAt       :: Array String
, requireFlags :: Boolean
, laxPlacement :: Boolean
  | r
}
