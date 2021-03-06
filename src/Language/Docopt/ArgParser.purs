module Language.Docopt.ArgParser (
    run
  , Result ()
  , module Reexport
  ) where

import Prelude
import Data.Either (Either(..))
import Data.List (List(), fromFoldable)
import Data.Either (Either())
import Data.Tuple (Tuple(), fst)
import Control.Monad.Transformerless.RWS (RWS(), evalRWS)
import Text.Parsing.Parser (ParseError, PState(PState), runParserT) as P
import Text.Parsing.Parser.Pos (initialPos) as P

import Language.Docopt.Specification
import Language.Docopt.Argument (Branch) as D
import Language.Docopt.Env (Env) as D
import Language.Docopt.ArgParser.Parser (spec, initialState, ValueMapping(),
                                        Options()) as P
import Language.Docopt.ArgParser.Lexer (lex, Options()) as L
import Language.Docopt.ArgParser.Parser (ValueMapping(), Options) as Reexport
import Language.Docopt.ArgParser.Parser (Options)

type Result = Tuple D.Branch (List P.ValueMapping)

run
  :: ∀ r
   . Specification -- ^ the specification
  -> D.Env         -- ^ the environment
  -> Array String  -- ^ the user input
  -> Options r
  -> Either P.ParseError Result
run spec env argv options = do
  toks <- L.lex (fromFoldable argv) options
  fst $ evalRWS
          (P.runParserT
            (P.PState toks P.initialPos)
            (P.spec spec options)
          )
          env
          P.initialState
