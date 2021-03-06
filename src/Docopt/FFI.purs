-- |
-- | Docopt FFI surface.
-- |
-- | Entrypoints to be called from JS-land.
-- | Data input and output is santized for either language and functions are
-- | curried/uncurried as needed.
-- |

module Docopt.FFI (
  run
, runFromSpec
, parse
, undefined
, readAsString
, readSpec
, specToForeign
) where

import Prelude
import Debug.Trace
import Data.Function.Uncurried
import Data.Bifunctor (lmap)
import Data.Maybe (Maybe(..), maybe, fromMaybe, isJust)
import Data.List (toUnfoldable, List(Nil), (:), fromFoldable)
import Data.String.Ext ((~~))
import Control.Monad.Eff.Exception (error, throwException, EXCEPTION())
import Data.Tuple (Tuple())
import Data.Foldable (intercalate)
import Data.Tuple.Nested ((/\))
import Data.Array (singleton, uncons, fromFoldable) as Array
import Data.Traversable (traverse, for)
import Data.NonEmpty ((:|))
import Control.Monad.Eff (Eff())
import Data.Either (Either(..), either)
import Data.StrMap (StrMap())
import Control.Bind ((=<<))
import Control.Alt (alt, (<|>))
import Data.String (toChar, toCharArray, singleton) as String
import Data.Foreign (readBoolean, readChar, readArray, readString, typeOf, toForeign,
                    readInt, readNumber, unsafeReadTagged) as F
import Data.Foreign (Foreign, F, ForeignError(..), typeOf, unsafeFromForeign,
                    toForeign)
import Data.Foreign.Class (readProp) as F
import Data.Foreign.NullOrUndefined as F
import Language.Docopt.Argument (Branch(), Argument(..), OptionArgument(..),
                                OptionArgumentObj)
import Language.Docopt.OptionAlias (OptionAlias())
import Language.Docopt.OptionAlias (OptionAlias(..)) as OptionAlias
import Unsafe.Coerce (unsafeCoerce)

import Docopt as Docopt
import Language.Docopt (Specification(), Docopt())
import Language.Docopt.Value (Value(..))

type RawValue = Unit

-- | Convert a Value into a JS-native value.
rawValue :: Value -> RawValue
rawValue (BoolValue   b) = unsafeCoerce b
rawValue (IntValue    i) = unsafeCoerce i
rawValue (FloatValue  x) = unsafeCoerce x
rawValue (StringValue s) = unsafeCoerce s
rawValue (ArrayValue xs) = unsafeCoerce $ rawValue <$> xs

foreign import isTruthy :: Foreign -> Boolean

-- |
-- | Run neodoc from JS.
-- |
run :: ∀ e.
  Fn2 String  -- ^ The neodoc text
      Foreign -- ^ The options (optional)
      (Eff (Docopt.DocoptEff e) (StrMap RawValue))
run = mkFn2 go
  where
    go helpText rOpts = do
      result <- Docopt.run (Right helpText) (readCommonOpts rOpts)
      pure $ rawValue <$> result

-- |
-- | Run neodoc from JS, provided a spec.
-- |
runFromSpec :: ∀ e.
  Fn2 Foreign -- ^ The neodoc spec
      Foreign -- ^ The options (optional)
      (Eff (Docopt.DocoptEff e) (StrMap RawValue))
runFromSpec = mkFn2 go
  where
    go rSpec rOpts = do
      case (readSpec rSpec) of
        Left e -> throwException (error (show e))
        Right spec -> do
          result <- Docopt.run (Left spec) (readCommonOpts rOpts)
          pure $ rawValue <$> result

-- | Interpret a foreign value as a JS dictionary
readObject :: Foreign -> F (StrMap Foreign)
readObject value | isObject value = pure $ unsafeFromForeign value
readObject value = Left (TypeMismatch "object" (typeOf value))

-- | Is this Foreign value an object?
isObject :: Foreign -> Boolean
isObject f = F.typeOf f == "object"

-- | Read common neodoc options
readCommonOpts :: Foreign -> Docopt.Options {}
readCommonOpts o = Docopt.defaultOptions {
    -- override argv with a custom array. Defaults to using `process.argv`
    argv = flip alt Docopt.defaultOptions.argv do
            toMaybe do
              F.readProp "argv" o

    -- override the environment with a custom hashmap. Defaults to using
    -- `process.env`
  , env = flip alt Docopt.defaultOptions.env do
            toMaybe do
              unsafeCoerce <$> do
                readObject =<< F.readProp "env" o

    -- set the version to be displayed when '--version' is issued.
  , version = flip alt Docopt.defaultOptions.version do
            toMaybe do
                F.readProp "version" o

    -- set the version flags that trigger 'version'
  , versionFlags = fromMaybe Docopt.defaultOptions.versionFlags do
            toMaybe do
                F.readProp "versionFlags" o

    -- set the version flags that trigger 'version'
  , helpFlags = fromMaybe Docopt.defaultOptions.helpFlags do
            toMaybe do
                F.readProp "helpFlags" o

    -- enable "options-first" parsing. Options are only parsed and
    -- validated until the first operand (positional or command) is met.
    -- Trailing options are collected into a designated placeholder.
  , optionsFirst = either (const Docopt.defaultOptions.optionsFirst) id
                    (isTruthy <$> do
                      F.readProp "optionsFirst" o)

    -- enable "smart-options" parsing. This causes singleton groups that
    -- "look like" they are describing an option to expand to such an
    -- option, e.g.: '[-o ARG]' becomes '[-o=ARG]'.
  , smartOptions = either (const Docopt.defaultOptions.smartOptions) id
                    (isTruthy <$> do
                      F.readProp "smartOptions" o)

    -- stop parsing at these custom EOA markers. This allows any option
    -- to terminate a parse and collect all subsequent args.
  , stopAt = fromMaybe Docopt.defaultOptions.stopAt do
      toMaybe do
        p <- F.readProp "stopAt" o
        unsafeCoerce (F.readArray p) <|> do
          Array.singleton <$> F.readString p

    -- require flags to be explicitly passed? By default neodoc
    -- ignores missing flags during parsing argv.
  , requireFlags = either (const Docopt.defaultOptions.requireFlags) id
                    (isTruthy <$> do
                      F.readProp "requireFlags" o)

    -- relax placement rules. Positionals and commands are no longer
    -- solid anchor points but can shift as well. The order amongst
    -- them, however, remains fixed.
  , laxPlacement = either (const Docopt.defaultOptions.laxPlacement) id
                    (isTruthy <$> do
                      F.readProp "laxPlacement" o)

    -- don't exit the process upon failure. By default, neodoc will
    -- exit the program if an error occured, right after printing the
    -- help text alongside an error message.
  , dontExit = either (const Docopt.defaultOptions.dontExit) id
                        (isTruthy <$> do
                          F.readProp "dontExit" o)
  }

-- |
-- | Parse the help-text and return the spec as a JS value
-- |
parse :: ∀ e.
        Fn2 String  -- ^ The neodoc help-text
            Foreign -- ^ The options (optional)
            (Eff (Docopt.DocoptEff e) ({
              specification :: Array (Array (Array Foreign))
            , shortHelp     :: String
            , program       :: String
            , help          :: String
            }))
parse = mkFn2 go
  where
    go helpText fOpts =
      let
        opts
          = Docopt.defaultOptions {
              -- enable "smart-options" parsing. This causes singleton groups
              -- that "look like" they are describing an option to expand to
              -- such an option, e.g.: '[-o ARG]' becomes '[-o=ARG]'.
              smartOptions
                = either (const Docopt.defaultOptions.smartOptions) id
                         (isTruthy <$> do
                           F.readProp "smartOptions" fOpts)
            }
       in specToForeign <$> Docopt.parse helpText opts

readOptionAlias :: String -> Either String OptionAlias
readOptionAlias s = case fromFoldable (String.toCharArray s) of
  '-' : '-' : '-' : _  -> Left "Must not start with more than 2 dashes"
  '-' : '-' : x   : xs -> Right $ OptionAlias.Long (reassemble x xs)
  '-' : x   : Nil      -> Right $ OptionAlias.Short x
  '-' : x   : _        -> Left "Single dash mandates single character name"
  _                    -> Left "Must start with a dash"
  where reassemble x xs = intercalate "" (String.singleton <$> (x:xs))

specToForeign
  :: Docopt
  -> { specification :: Array (Array (Array Foreign))
     , shortHelp     :: String
     , program       :: String
     , help          :: String
     }
specToForeign { help, shortHelp, specification, program } =
    let
      jsSpecification = toUnfoldable do
        specification <#> \branches -> do
          toUnfoldable do
            convBranch <$> branches

    in {
      help:          help
    , program:       program
    , shortHelp:     shortHelp
    , specification: jsSpecification
    }

    where
    convBranch :: Branch -> Array Foreign
    convBranch args = toUnfoldable $ convArg <$> args

    convArg :: Argument -> Foreign
    convArg (EOA)          = F.toForeign { type: "EOA" }
    convArg (Stdin)        = F.toForeign { type: "Stdin" }
    convArg (Command x)    = F.toForeign { type: "Command", value: x }
    convArg (Positional x) = F.toForeign { type: "Positional", value: x }
    convArg (Group x)      = F.toForeign {
      type: "Group"
    , value: {
        optional:   x.optional
      , repeatable: x.repeatable
      , branches:   (toUnfoldable $ convBranch <$> x.branches) :: Array (Array Foreign)
      }
    }
    convArg (Option x) = F.toForeign {
      type: "Option"
    , value: {
        aliases:    Array.fromFoldable $ x.aliases <#> case _ of
                      OptionAlias.Short f ->  "-" ~~ String.singleton f
                      OptionAlias.Long  n -> "--" ~~ n
      , env:        maybe undefined F.toForeign x.env
      , repeatable: x.repeatable
      , arg:        maybe undefined (\(OptionArgument a) -> {
                      name:     a.name
                    , default:  maybe undefined rawValue a.default
                    , optional: a.optional
                    }) x.arg
      }
    }

-- |
-- | Parse a foreign value into a specification
-- |
readSpec :: Foreign -> F Docopt
readSpec input = do
  help                      <- F.readProp "help" input
  program                   <- F.readProp "program" input
  shortHelp                 <- F.readProp "shortHelp" input
  toplevel :: Array Foreign <- F.readProp "specification" input
  spec <- fromFoldable <$> do
    for toplevel \usage -> do
      branches <- F.readArray usage
      fromFoldable <$> do
        for branches \branch -> do
          args <- F.readArray branch
          fromFoldable <$> do
            for args readArg
  pure {
    help:          help
  , program:       program
  , shortHelp:     shortHelp
  , specification: spec
  }

  where
  readNode :: Foreign -> F (Tuple String (Maybe Foreign))
  readNode o = (/\) <$> (F.readString =<< F.readProp "type" o)
                    <*> ((Just <$> F.readProp "value" o) <|> pure Nothing)

  readArg :: Foreign -> F (Argument)
  readArg o = do
    n <- readNode o
    case n of
      "Group" /\ (Just v) -> do
        grp <$> (isTruthy <$> F.readProp "optional" v)
            <*> (isTruthy <$> F.readProp "repeatable" v)
            <*> (fromFoldable <$> do
                  branches <- F.readArray =<< F.readProp "branches" v
                  for branches \branch -> do
                    args <- F.readArray branch
                    fromFoldable <$> do
                      for args readArg
                )
      "Command" /\ (Just v) -> do
        co <$> (readAsString =<< F.readProp "name" v)
           <*> (isTruthy <$> F.readProp "repeatable" v)
      "Positional" /\ (Just v) -> do
        po <$> (readAsString =<< F.readProp "name" v)
           <*> (isTruthy <$> F.readProp "repeatable" v)
      "Option" /\ (Just v) -> do
        opt
          <$> (do
            aliases :: Array Foreign <- F.readProp "aliases" v
            ns <- for (fromFoldable aliases) \alias -> do
              s <- readAsString alias
              lmap  (\e -> JSONError $ "Invalid option alias " ~~ show s  ~~ ": " ~~ e)
                    (readOptionAlias s)
            case ns of
              x : xs -> pure (x :| xs)
              _      -> Left $ JSONError "Option must at least have one alias"
          )
          <*> (isTruthy <$> F.readProp "repeatable" v)
          <*> (ifHasProp v "env" readAsString)
          <*> (readOptArg v)
      "Stdin" /\ _ -> pure Stdin
      "EOA"   /\ _ -> pure EOA
      x /\ _ -> Left (TypeMismatch "Group, Command, Positional, Option, Stdin or EOA" x)

  readOptArg :: Foreign -> F (Maybe OptionArgument)
  readOptArg v = "option-argument" <?> do
    mx <- nullOrUndefined (F.readProp "arg" v)
    case mx of
      Nothing -> pure Nothing
      Just x  -> Just <<< OptionArgument <$> do
          arg <$> (ifHasProp' x "name" "<ARG>" readAsString)
              <*> (ifHasProp  x "default" readValue)
              <*> (isTruthy <$> F.readProp "optional" x)

  readFlag :: Foreign -> F Char
  readFlag v = "flag. Must be a single character." <?> do
      s <- readAsString v
      F.readChar (F.toForeign s)

  grp x y z    = Group      { optional: x, repeatable: y, branches: z }
  co  x y      = Command    { name: x, repeatable: y }
  po  x y      = Positional { name: x, repeatable: y }
  opt as r e a = Option     { aliases: as, repeatable: r, env: e, arg: a }
  arg x y z    = { name: x, default: y, optional: z }

readValue :: Foreign -> F Value
readValue x =
  (BoolValue   <$> F.readBoolean x) <|>
  (IntValue    <$> F.readInt     x) <|>
  (FloatValue  <$> F.readNumber  x) <|>
  (StringValue <$> F.readString  x) <|>
  (ArrayValue  <$> (F.readArray x >>= \vs -> for vs readValue))

optional :: ∀ a. F a -> F (Maybe a)
optional x = (Just <$> x) <|> pure Nothing

ifHasProp :: ∀ a. Foreign -> String -> (Foreign -> F a) -> F (Maybe a)
ifHasProp v s f = do
  mv <- nullOrUndefined (F.readProp s v)
  case mv of
    Nothing -> pure Nothing
    Just  v -> Just <$> f v

ifHasProp' :: ∀ a. Foreign -> String -> a -> (Foreign -> F a) -> F a
ifHasProp' v s o f = do
  mv <- ifHasProp v s f
  pure (fromMaybe o mv)

toMaybe :: ∀ a b. Either a b -> Maybe b
toMaybe e = either (const Nothing) (pure <<< id) e

readAsString :: Foreign -> F String
readAsString v = do
  (             (F.unsafeReadTagged "String"  v :: F String)) <|>
  (toString <$> (F.unsafeReadTagged "Boolean" v :: F String)) <|>
  (toString <$> (F.unsafeReadTagged "Number"  v :: F String))

nullOrUndefined :: F Foreign -> F (Maybe Foreign)
nullOrUndefined x = F.unNullOrUndefined <$> do
  F.readNullOrUndefined pure =<< x

infixl 9 expected as <?>
expected :: ∀ a. String -> F a -> F a
expected msg x = lmap (\_ -> JSONError $ "Invalid " <> msg) x

foreign import undefined :: ∀ a. a
foreign import toString  :: ∀ a. a -> String
