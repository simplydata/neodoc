module Language.Docopt.Parser.Usage.Argument (
    Argument (..)
  , IsRepeatable ()
  , IsOptional ()
  , Branch ()
  , prettyPrintArg
  , prettyPrintBranch
  , sopt, sopt_, soptR, soptR_
  , lopt, lopt_, loptR, loptR_
  , eoa
  , co
  , po
  , poR
  , stdin
  ) where

import Prelude
import Data.Maybe (Maybe(..))
import Data.List (List(..))
import Data.Foldable (intercalate)
import qualified Language.Docopt.Parser.Usage.Option as O

type IsRepeatable = Boolean
type IsOptional = Boolean
type Branch = List Argument
data Argument
  = Command     String
  | Positional  String IsRepeatable
  | Option      O.LOpt
  | OptionStack O.SOpt
  | Group       IsOptional (List Branch) IsRepeatable
  | EOA
  | Stdin

instance showArgument :: Show Argument where
  show (EOA)            = "--"
  show (Stdin)          = "-"
  show (Command n)      = "Command " ++ n
  show (Positional n b) = "Positional " ++ n ++ " " ++ show b
  show (Option o)       = "Option " ++ show o
  show (OptionStack o)  = "OptionStack " ++ show o
  show (Group n b o)    = "Group " ++ show n ++ " " ++ show b ++ " " ++ show o

instance eqArgument :: Eq Argument where
  eq (Stdin)          (Stdin)            = true
  eq (EOA)            (EOA)              = true
  eq (Command s)      (Command s')       = (s == s')
  eq (Positional s r) (Positional s' r') = (s == s') && (r == r')
  eq (Option o)       (Option o')        = o == o'
  eq (Group b xs r)   (Group b' xs' r')  = (b == b') && (xs == xs') && (r == r')
  eq (OptionStack o)  (OptionStack o')   = o == o'
  eq _                _                  = false

prettyPrintBranch :: Branch -> String
prettyPrintBranch xs = intercalate " " (prettyPrintArg <$> xs)

prettyPrintArg :: Argument -> String
prettyPrintArg (Command n) = n
prettyPrintArg (Positional n r)
  = n ++ if r then "..." else ""
prettyPrintArg (Option o) = O.prettyPrintLOpt o
prettyPrintArg (OptionStack o) = O.prettyPrintSOpt o
prettyPrintArg (Group b xs r)
  =  (if b then "(" else "[")
  ++ (intercalate " | " (prettyPrintBranch <$> xs))
  ++ (if b then ")" else "]")
  ++ (if r then "..." else "")
prettyPrintArg (EOA) = "--"
prettyPrintArg (Stdin) = "-"

-- short hand to create a short option node
sopt :: Char -> Array Char -> String -> Argument
sopt f fs a = OptionStack $ O.sopt f fs a

sopt_ :: Char -> Array Char -> Argument
sopt_ f fs = OptionStack $ O.sopt_ f fs

soptR :: Char -> Array Char -> String -> Argument
soptR f fs a = OptionStack $ O.soptR f fs a

soptR_ :: Char -> Array Char -> Argument
soptR_ f fs = OptionStack $ O.soptR_ f fs

-- short hand to create a long option node
lopt :: String -> String -> Argument
lopt n a = Option $ O.lopt n a

lopt_ :: String -> Argument
lopt_ n = Option $ O.lopt_ n

loptR :: String -> String -> Argument
loptR n a = Option $ O.loptR n a

loptR_ :: String -> Argument
loptR_ n = Option $ O.loptR_ n

-- short hand to create an end-of-argument marker
eoa :: Argument
eoa = EOA

-- short hand to create an stdin marker
stdin :: Argument
stdin = Stdin

-- short hand to create a command node
co :: String -> Argument
co = Command

-- short hand to create a positional node
po :: String -> Argument
po n = Positional n false

poR :: String -> Argument
poR n = Positional n true
