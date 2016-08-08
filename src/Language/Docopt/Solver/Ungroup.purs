-- | Simplify groupings.
-- |
-- | This is somewhwat similar to the group simplification in the original
-- | docopt except that this implementation will retain more (if not all) of
-- | the semantics of the input.
-- |
-- | Example:
-- |
-- | [[-a]]          -> [-a]
-- | [[-a] [-b]]     -> [-a -b]
-- | [[-a] | [-b]]   -> [-a | -b]
-- | [-a -b -c]      -> [-a] [-b] [-c]
-- | [-a -b -c | -d] -> [-a -b -c | -d]
-- |
-- | Rules:
-- | * Nested singleton group reduction (OR `on` `_.optional`):
-- |    * ([...]) -> [...]
-- |    * [(...)] -> [...]
-- |    * [[...]] -> [...]
-- |    * ((...)) -> (...)
-- | * Non-singleton groups remain in-tact
-- |    * [-a | -b] -> [-a | -b]
-- |

module Language.Docopt.Solver.Ungroup where

import Prelude
import Data.List (List(Nil), singleton)
import Language.Docopt

unGroup :: Specification -> Specification
unGroup spec = do
  usage <- spec
  pure do
    branch <- usage
    pure do
      arg <- branch
      unGroupArg topLevelContainer arg

  where
    topLevelContainer = { optional: false, repeatable: false }

type Container = {
  optional   :: Boolean
, repeatable :: Boolean
}

-- | Simplify a single argument
unGroupArg
  :: Container -- ^ The containing argument
  -> Argument  -- ^ The argument to simplify
  -> List Argument
unGroupArg _ _ = Nil
