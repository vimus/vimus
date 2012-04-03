{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses, FlexibleContexts, ScopedTypeVariables, CPP #-}
module Command.Core (
  Command (..)
, commandHelp
, Argument (..)
, Action (..)
, VimusAction
, runAction
, command
, command0
, command1
, command2
, command3

#ifdef TEST
, argumentErrorMessage
, readParser
, IsAction (..)
#endif
) where

import           Vimus (Vimus, printError)
import           Util (maybeRead)
import           Control.Applicative
import           Data.Char
import           Data.List (intercalate)

import           WindowLayout (WindowColor(..), defaultColor)
import           UI.Curses (Color, black, red, green, yellow, blue, magenta, cyan, white)
import           Command.Parser

runAction :: Action a -> String -> Either String a
runAction action s = case runParser (unAction action <* skipWhile isSpace) s of
  Right (a, "") -> Right a
  Right (_, xs) -> Left ("superfluous argument: " ++ show xs)
  Left err -> Left err

argumentErrorMessage
  :: Int      -- ^ expected number of arguments
  -> [String] -- ^ actual arguments
  -> String
argumentErrorMessage n args =
  case drop n args of
    []  ->  reqMessage
    [x] -> "unexpected argument: " ++ x
    xs  -> "unexpected arguments: " ++ unwords xs
  where
    reqMessage
      | n == 1    = "one argument required"
      | n == 2    = "two arguments required"
      | n == 2    = "three arguments required"
      | otherwise = show n ++ " arguments required"

type VimusAction = Action (Vimus ())
newtype Action a = Action {unAction :: Parser a}

class IsAction a b where
  toAction :: a -> Action b
  actionArguments :: a -> b -> [String]

instance IsAction a a where
  toAction = Action . return
  actionArguments _ _ = []

instance (Argument a, IsAction b c) => IsAction (a -> b) c where
  toAction action = Action $ argumentParser >>= unAction . toAction . action
  actionArguments _ _ = argumentName (undefined :: a) : actionArguments (undefined :: b) (undefined :: c)

-- | A command.
data Command = Command {
  commandName      :: String
, commandArguments :: [String]
, commandAction    :: VimusAction
}

commandHelp :: Command -> String
commandHelp c = intercalate " " $ commandName c : map (\x -> "{" ++ x ++ "}") (commandArguments c)

-- | Define a command.
command :: forall a . IsAction a (Vimus ()) => String -> a -> Command
command name action = Command name (actionArguments action (undefined :: Vimus ())) (toAction action)

-- | Define a command that takes no arguments.
command0 :: String -> Vimus () -> Command
command0 = command

-- | Define a command that takes one argument.
command1 :: (Argument a) => String -> (a -> Vimus ()) -> Command
command1 = command

-- | Define a command that takes two arguments.
command2 :: (Argument a, Argument b) => String -> (a -> b -> Vimus ()) -> Command
command2 = command

-- | Define a command that takes three arguments.
command3 :: (Argument a, Argument b, Argument c) => String -> (a -> b -> c -> Vimus ()) -> Command
command3 = command


-- | An argument.
class Argument a where
  argumentName   :: a -> String
  argumentParser :: Parser a

-- | A parser for arguments in the Read class.
readParser :: forall a . (Read a, Argument a) => Parser a
readParser = mkParser maybeRead

-- | A helper function for constructing argument parsers.
mkParser :: forall a . (Argument a) => (String -> Maybe a) -> Parser a
mkParser f = do
  r <- takeWord <|> missing
  maybe (err r) return (f r)
  where
    name    = argumentName (undefined :: a)
    missing = parserFail ("missing required argument: " ++ name)
    err x   = parserFail ("Argument '" ++ x ++ "' is not a valid " ++ name ++ "!")

-- | Skip any whitespace and then return all non-whitespace.
--
-- Fail on input that only consists of whitespace.
takeWord :: Parser String
takeWord = skipWhile isSpace *> takeWhile1 (not . isSpace)

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
  argumentName   = const "color name"
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
