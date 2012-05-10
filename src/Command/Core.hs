{-# LANGUAGE CPP, TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses, FlexibleContexts, ScopedTypeVariables #-}
module Command.Core (
  Command (..)
, commandSynopsis
, Argument (..)
, Action (..)
, VimusAction
, runAction
, command

-- * Helpers for defining @Argument@ instances
, missingArgument
, invalidArgument
, specificArgumentError

#ifdef TEST
, readParser
, IsAction (..)
#endif
) where

import           Vimus (Vimus)
import           Util (maybeRead)
import           Control.Applicative
import           Control.Monad (unless)
import           Data.Char

import           WindowLayout (WindowColor(..), defaultColor)
import           UI.Curses (Color, black, red, green, yellow, blue, magenta, cyan, white)
import           Command.Type
import           Command.Parser

runAction :: Action a -> String -> Either String a
runAction action s = either (Left . show) (Right . fst) $ runParser (unAction action <* endOfInput) s


class IsAction a b where
  toAction :: a -> Action b
  actionArguments :: a -> b -> [String]

instance IsAction a a where

  toAction a = Action $ do
    r <- takeInput
    unless (null r) $ do
      parserFail (SuperfluousInput r)
    return a

  actionArguments _ _ = []

instance (Argument a, IsAction b c) => IsAction (a -> b) c where
  toAction action = Action $ (argumentParser <* skipWhile isSpace) >>= unAction . toAction . action
  actionArguments _ _ = argumentName (undefined :: a) : actionArguments (undefined :: b) (undefined :: c)


-- | Get help text for given command.
commandSynopsis :: Command -> String
commandSynopsis c = unwords $ commandName c : map (\x -> "{" ++ x ++ "}") (commandArguments c)

-- | Define a command.
command :: forall a . IsAction a (Vimus ()) => String -> String -> a -> Command
command name description action = Command name description (actionArguments action (undefined :: Vimus ())) (toAction action)

-- | An argument.
class Argument a where
  argumentName   :: a -> String

  -- | A parser for this argument.
  --
  -- The parser can assume that the input is either empty or starts with a
  -- non-whitespace character.
  argumentParser :: Parser a

-- | A parser for arguments in the Read class.
readParser :: forall a . (Read a, Argument a) => Parser a
readParser = mkParser maybeRead

-- | A helper function for constructing argument parsers.
mkParser :: forall a . (Argument a) => (String -> Maybe a) -> Parser a
mkParser f = do
  r <- takeWhile1 (not . isSpace) <|> missingArgument (undefined :: a)
  maybe (invalidArgument (undefined ::a) r) return (f r)

-- | A failing parser that indicates a missing argument.
missingArgument :: Argument a => a -> Parser b
missingArgument = parserFail . MissingArgument . argumentName

-- | A failing parser that indicates an invalid argument.
invalidArgument :: Argument a => a -> Value -> Parser b
invalidArgument t = parserFail . InvalidArgument (argumentName t)

-- | A failing parser that indicates a specific error.  It takes precedence
-- over any other kind of error.
specificArgumentError :: String -> Parser b
specificArgumentError = parserFail . SpecificArgumentError

instance Argument Int where
  argumentName   = const "int"
  argumentParser = readParser

instance Argument Integer where
  argumentName   = const "integer"
  argumentParser = readParser

instance Argument Float where
  argumentName   = const "float"
  argumentParser = readParser

instance Argument Double where
  argumentName   = const "double"
  argumentParser = readParser

instance Argument String where
  argumentName   = const "string"
  argumentParser = mkParser Just

instance Argument WindowColor where
  argumentName   = const "item"
  argumentParser = mkParser parse
    where
      parse input = case map toLower input of
        "main"           -> Just MainColor
        "ruler"          -> Just RulerColor
        "tab"            -> Just TabColor
        "input"          -> Just InputColor
        "playstatus"     -> Just PlayStatusColor
        "songstatus"     -> Just SongStatusColor
        "error"          -> Just ErrorColor
        "suggestions"    -> Just SuggestionsColor
        _                -> Nothing

instance Argument Color where
  argumentName   = const "color"
  argumentParser = mkParser parse
    where
      parse input = case map toLower input of
        "default" -> Just defaultColor
        "black"   -> Just black
        "red"     -> Just red
        "green"   -> Just green
        "yellow"  -> Just yellow
        "blue"    -> Just blue
        "magenta" -> Just magenta
        "cyan"    -> Just cyan
        "white"   -> Just white
        _         -> Nothing
