module Language.Docopt.ParserGen.Pretty where

import Prelude
import Data.Maybe (maybe)
import Data.Foldable (intercalate)
import Data.String (fromCharArray)
import Language.Docopt.ParserGen.Types
import qualified Language.Docopt.Pretty as D
import qualified Data.Array as A

prettyPrintToken :: Token -> String
prettyPrintToken (EOA xs) = "-- " ++ intercalate " " (D.prettyPrintValue <$> xs)
prettyPrintToken (Lit s) = show s
prettyPrintToken (LOpt n a) = "--" ++ n ++ arg
  where arg = maybe "" ("=" ++) a
prettyPrintToken (SOpt n s a) = "-"  ++ (fromCharArray (A.cons n s)) ++ arg
  where arg = maybe "" ("=" ++) a