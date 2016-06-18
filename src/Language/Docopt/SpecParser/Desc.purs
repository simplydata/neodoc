module Language.Docopt.SpecParser.Desc (
    Desc (..)
  , Name (..)
  , OptionObj ()
  , PositionalObj ()
  , OptionArgumentObj ()
  , getFlag
  , getName
  , prettyPrintDesc
  , run
  , parse
  ) where

import Prelude
import Debug.Trace
import Data.Tuple (Tuple (Tuple))
import Data.Tuple (swap) as Tuple
import Data.Functor (($>))
import Data.Function (on)
import Data.String as Str
import Control.Lazy (defer)
import Control.Bind ((>=>))
import Control.Monad (when)
import Control.Alt ((<|>))
import Control.Apply ((*>), (<*))
import Control.MonadPlus (guard)
import Data.List (List(..), (:), many, some, head, length, filter, catMaybes,
                  reverse, fromList, toList)
import Text.Parsing.Parser (ParseError, fail) as P
import Text.Parsing.Parser.Combinators ((<?>), try, choice, lookAhead, manyTill,
                                        option, optionMaybe, notFollowedBy,
                                        (<??>)) as P
import Data.Either (Either(..), either)
import Data.Maybe (Maybe(Nothing, Just), isJust, isNothing, maybe, fromMaybe)
import Data.Generic (class Generic, gEq, gShow)
import Data.String (fromChar)
import Data.Array as A
import Data.String.Ext ((^=))

import Language.Docopt.Value (Value, prettyPrintValue)
import Language.Docopt.SpecParser.Common (sameIndent, markIndent, indented,
                                     moreIndented, lessIndented)
import Language.Docopt.SpecParser.Lexer (lexDescs)
import Language.Docopt.SpecParser.Lexer as L
import Language.Docopt.Value as Value

-- XXX: This is duplicated from Solver.purs.
--      Where should this live???
posArgsEq :: String -> String -> Boolean
posArgsEq = eq `on` (Str.toUpper <<< stripAngles)
infixl 9 posArgsEq as ^=^

stripAngles :: String -> String
stripAngles = stripPrefix <<< stripSuffix
  where
  stripPrefix s = fromMaybe s (Str.stripPrefix "<" s)
  stripSuffix s = fromMaybe s (Str.stripSuffix ">" s)

data Desc
  = OptionDesc     OptionObj
  | PositionalDesc PositionalObj

data Name
  = Flag Char
  | Long String
  | Full Char String

type PositionalObj = {
  name       :: String
, repeatable :: Boolean
, choices    :: List String
}

type OptionObj = {
  name       :: Name
, arg        :: Maybe OptionArgumentObj
, env        :: Maybe String
, repeatable :: Boolean
}

showOptionObj :: OptionObj -> String
showOptionObj o = "{ name: "       <> show o.name
               <> ", arg: "        <> show (OptionArgument <$> o.arg)
               <> ", env: "        <> show o.env
               <> ", repeatable: " <> show o.repeatable
               <> "}"

showPositionalObj :: PositionalObj -> String
showPositionalObj o  = "{ name: "       <> show o.name
                    <> ", choices: "    <> show o.choices
                    <> ", repeatable: " <> show o.repeatable
                    <> "}"

eqOptionObj :: OptionObj -> OptionObj -> Boolean
eqOptionObj o o' = o.name                     == o'.name
                && (OptionArgument <$> o.arg) == (OptionArgument <$> o'.arg)
                && o.env                      == o'.env
                && o.repeatable               == o'.repeatable

eqPositionalObj :: PositionalObj -> PositionalObj -> Boolean
eqPositionalObj p p' = p.name       == p'.name
                    && p.choices    == p'.choices
                    && p.repeatable == p'.repeatable

type OptionArgumentObj = {
  name     :: String
, default  :: Maybe Value
, optional :: Boolean
, choices  :: List String
}

showOptionArgumentObj :: OptionArgumentObj -> String
showOptionArgumentObj o = "{ name: "     <> show o.name
                       <> ", default: "  <> show o.default
                       <> ", optional: " <> show o.optional
                       <> ", choices: "  <> show o.choices
                       <> "}"

eqOptionArgumentObj :: OptionArgumentObj -> OptionArgumentObj -> Boolean
eqOptionArgumentObj a a' = a.name     == a'.name
                        && a.default  == a'.default
                        && a.optional == a'.optional
                        && a.choices  == a'.choices

newtype OptionArgument = OptionArgument OptionArgumentObj

unOptionArgument :: OptionArgument -> OptionArgumentObj
unOptionArgument (OptionArgument a) = a

instance showOptionArgument :: Show OptionArgument where
  show = showOptionArgumentObj <<< unOptionArgument

instance eqOptionArgument :: Eq OptionArgument where
  eq = eqOptionArgumentObj `on` unOptionArgument

getFlag :: Name -> Maybe Char
getFlag (Flag f)   = pure f
getFlag (Full f _) = pure f
getFlag _          = Nothing

getName :: Name -> Maybe String
getName (Long   n) = pure n
getName (Full _ n) = pure n
getName _          = Nothing

data Content
  = Default String
  | Env     String
  | Choice  String

isDefaultTag :: Content -> Boolean
isDefaultTag (Default _) = true
isDefaultTag _           = false

isChoiceTag :: Content -> Boolean
isChoiceTag (Choice _) = true
isChoiceTag _          = false

getDefaultValue :: Content -> Maybe Value
getDefaultValue (Default v) = either (const Nothing) Just (Value.parse v true)
getDefaultValue _           = Nothing

getChoices :: Content -> List String
getChoices (Choice s) = toList $ Str.trim <$> Str.split "," s
getChoices _ = Nil

isEnvTag :: Content -> Boolean
isEnvTag (Env _) = true
isEnvTag _       = false

getEnvKey :: Content -> Maybe String
getEnvKey (Env k) = Just k
getEnvKey _       = Nothing

derive instance genericName    :: Generic Name
derive instance genericContent :: Generic Content

instance showName     :: Show Name     where show = gShow
instance showContent  :: Show Content  where show = gShow
instance eqName       :: Eq Name       where eq = gEq

prettyPrintDesc :: Desc -> String
prettyPrintDesc (OptionDesc opt) = "Option " <> prettyPrintOption opt
prettyPrintDesc (PositionalDesc pos) = "Positional " <> prettyPrintPositional pos

instance showDesc :: Show Desc where
  show (OptionDesc o)        = "OptionDesc "     <> showOptionObj o
  show (PositionalDesc pos)  = "PositionalDesc " <> showPositionalObj pos

instance eqDesc :: Eq Desc where
  eq (OptionDesc o) (OptionDesc o')         = eqOptionObj o o'
  eq (PositionalDesc p) (PositionalDesc p') = eqPositionalObj p p'
  eq _             _                        = false

prettyPrintOption :: OptionObj -> String
prettyPrintOption opt
  = (name opt.name) <> arg <> env
  where
  name (Flag c)   = "-"  <> fromChar c
  name (Long n)   = "--" <> n
  name (Full c n) = "-"  <> fromChar c <> ", --" <> n

  arg = fromMaybe "" do
    a <- opt.arg
    pure $
      (if a.optional then "[" else "")
        <> "=" <> a.name
        <> (if a.optional then "]" else "")
        <> (if opt.repeatable then "..." else "")
        <> (maybe ""
                  (\v -> "[default: " <> prettyPrintValue v <> "]")
                  a.default)
        <> (if length a.choices == 0
              then ""
              else " [choices: " <> Str.joinWith "," (fromList a.choices)  <> "]")

  env = fromMaybe "" do
    k <- opt.env
    pure $ " [env: " <> k <> "]"

prettyPrintPositional :: PositionalObj -> String
prettyPrintPositional pos = pos.name
                        <> (if pos.repeatable then "..." else "")
                        <> choices
  where
  choices = maybe "" id do
    guard (length pos.choices > 0)
    pure $ " [choices: " <> Str.joinWith "," (fromList pos.choices)  <> "]"

prettyPrintOptionArgument :: OptionArgumentObj -> String
prettyPrintOptionArgument { optional: o, name: n, default: d, choices: c }
  = (if o then "[" else "") <> n <> (if o then "]" else "")
    <> maybe "" (\v -> " [default: " <> (prettyPrintValue v) <>  "]") d
    <> (if length c == 0
          then ""
          else " [choices: " <> Str.joinWith "," (fromList c)  <> "]")

run :: String -> Either P.ParseError (List Desc)
run = lexDescs >=> parse

parse :: (List L.PositionedToken) -> Either P.ParseError (List Desc)
parse = flip L.runTokenParser descParser

descParser :: L.TokenParser (List Desc)
descParser = markIndent do
  reverse <$> go Nil
  <* L.eof
  where
    go vs = do
      v <- (Just <$> desc) <|> (descContent true $> Nothing)
      case v of
        Just v' -> go (v' : vs)
        Nothing ->
          P.choice [
            desc >>= \v' -> go (v' : vs)
          , pure vs
          ]

    anyName :: L.TokenParser String
    anyName = L.angleName <|> L.shoutName <|> L.name

    desc :: L.TokenParser Desc
    desc = defer \_-> "--option or <positional> description" P.<??> do
            P.choice $ [ optionDesc
                       , positionalsDesc
                       ]

    descContent :: Boolean -> L.TokenParser (List Content)
    descContent toplevel = do
      markIndent do
        catMaybes <$> (flip P.manyTill descEnd do
          P.choice $ P.try <$> [
            Just <<< Default <$> L.tag "default"
          , Just <<< Env     <$> L.tag "env"
          , Just <<< Choice  <$> L.tag "choices"
          , L.anyToken $> Nothing
          ])
      <* (void L.eof <|> void (some L.newline))
      where
        descEnd = do
          P.choice [
            L.eof
          , void $ P.lookAhead do
              L.newline
              when (not toplevel)
                lessIndented
              P.choice [
                void L.sopt
              , void L.lopt
              , void L.angleName
              , void L.shoutName
              ]
          ]

    positionalsDesc :: L.TokenParser Desc
    positionalsDesc = do
      name        <- L.angleName <|> L.shoutName
      repeatable  <- P.option false $ L.tripleDot $> true
      description <- descContent false
      let choices = getChoices <$> filter isChoiceTag description

      if (length choices > 1)
         then P.fail $ name <> " has multiple [choices] tags"
         else pure unit

      pure $ PositionalDesc {
        name:       name
      , repeatable: repeatable
      , choices:    fromMaybe Nil (head choices)
      }

    optionDesc :: L.TokenParser Desc
    optionDesc = do

      xopt        <- opt
      description <- descContent false

      let defaults = getDefaultValue <$> filter isDefaultTag description
          envs     = getEnvKey       <$> filter isEnvTag     description
          choices  = getChoices      <$> filter isChoiceTag  description

      if (length defaults > 1)
         then P.fail $
          "Option " <> (show $ prettyPrintOption xopt)
                    <> " has multiple defaults!"
         else pure unit

      if (length envs > 1)
         then P.fail $
          "Option " <> (show $ prettyPrintOption xopt)
                    <> " has multiple environment mappings!"
         else pure unit

      if (length choices > 1)
         then P.fail $
          "Option " <> (show $ prettyPrintOption xopt)
                    <> " has multiple [choices] tags"
         else pure unit

      let default = head defaults >>= id
          env     = head envs     >>= id
          choice  = fromMaybe Nil (head choices)

      if (isJust default) && (isNothing xopt.arg)
         then P.fail $
          "Option " <> (show $ prettyPrintOption xopt)
                    <> " does not take arguments. "
                    <> "Cannot specify defaults."
         else pure unit

      pure $ OptionDesc $
        xopt  { env = env
              , arg = do
                  arg <- xopt.arg
                  pure $ arg {
                    default = default
                  , choices = choice
                  }
              }

      where

        short :: L.TokenParser OptionObj
        short = do
          opt <- do
            opt <- L.sopt
            (guard $ (A.length opt.stack == 0)) P.<?> "No stacked options"
            pure { flag: opt.flag, arg: opt.arg }

          -- Grab the adjacent positional-looking argument
          -- in case the token did not have an explicit
          -- binding via `=`.
          arg <- maybe
                  (P.optionMaybe do
                    n <- L.shoutName <|> L.angleName
                    pure { name: n, optional: false }
                  )
                  (pure <<< Just)
                  opt.arg

          repeatable <- P.option false $ L.tripleDot $> true

          pure $  { name: Flag opt.flag
                  , arg:  do
                      a <- arg
                      pure {
                        name:     a.name
                      , optional: a.optional
                      , choices:  Nil
                      , default:  Nothing
                      }
                  , env:        Nothing
                  , repeatable: repeatable
                  }

        long :: L.TokenParser OptionObj
        long = do
          opt <- L.lopt

          -- Grab the adjacent positional-looking argument
          -- in case the token did not have an explicit
          -- binding via `=`.
          arg <- maybe
                  (P.optionMaybe do
                    n <- L.shoutName <|> L.angleName
                    pure { name: n, optional: false }
                  )
                  (pure <<< Just)
                  opt.arg

          repeatable <- P.option false $ L.tripleDot $> true

          pure $  { name: Long opt.name
                  , arg:  do
                      a <- arg
                      pure {
                        name:     a.name
                      , optional: a.optional
                      , choices:  Nil
                      , default:  Nothing
                      }
                  , env:        Nothing
                  , repeatable: repeatable
                  }

        opt :: L.TokenParser OptionObj
        opt = do
          let p = P.choice  [ Left  <$> short
                            , Right <$> long
                            ]
          x <- p
          y <- P.optionMaybe
                $ P.choice  [ L.comma *> many L.newline *> indented *> p
                            , p
                            ]

          case Tuple x y of
            Tuple (Left s)  (Just (Right l)) -> combine s l
            Tuple (Right l) (Just (Left s))  -> combine s l
            Tuple (Left s)  Nothing          -> pure s
            Tuple (Right l) Nothing          -> pure l
            Tuple (Left { name: Flag f  })
                  (Just (Left { name: Flag f' }))  -> P.fail $
              "Expected an optional long alias for -"
                <> fromChar f <> ", but got: -" <> fromChar f'
            Tuple (Right { name: Long n  })
                  (Just (Right { name: Long n' })) -> P.fail $
              "Expected an optional short alias for --"
                <> n <> ", but got: --" <> n'
            otherwise -> P.fail "Expected options"

          where
            -- Combine two options into one. This function *does not* cover all
            -- cases right now. It deals only with a known subset and can there-
            -- fore make assumptions
            combine :: OptionObj -> OptionObj -> L.TokenParser OptionObj
            combine (x@{ name: Flag f }) (y@{ name: Long n }) = do
              either P.fail pure do
                arg <- combineArg x.arg y.arg
                pure $ {
                  name:       Full f n
                , arg:        arg
                , env:        Nothing -- No need to keep at this stage
                , repeatable: x.repeatable || y.repeatable
                }
              where
                combineArg (Just a) (Just a')
                  | (a.name ^=^ a'.name) = pure $ Just
                      $ { name:     a.name
                        , optional: a.optional || a'.optional
                        , default:  a.default <|> a'.default
                        , choices:  Nil -- No need to keep at this stage
                        }
                combineArg Nothing  (Just b) = pure (pure b)
                combineArg (Just a) Nothing  = pure (pure a)
                combineArg Nothing Nothing   = pure Nothing
                combineArg (Just a) (Just b) = Left $
                        "Option-arguments mismatch: "
                          <> (show $ prettyPrintOptionArgument a)
                          <> " and "
                          <> (show $ prettyPrintOptionArgument b)
            combine _ _ = P.fail "Invalid case - expected flag and long option"
