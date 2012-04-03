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

runAction :: String -> VimusAction -> Vimus ()
runAction s action = either printError id (unAction action s)

argumentError
  :: Int      -- ^ expected number of arguments
  -> [String] -- ^ actual arguments
  -> Vimus ()
argumentError n = printError . argumentErrorMessage n

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
newtype Action a = Action {unAction :: String -> Either String a}

class IsAction a b where
  toAction :: a -> Action b
  actionArguments :: a -> b -> [String]

instance IsAction a a where
  toAction action = Action $ \input -> case dropWhile isSpace input of
    "" -> Right action
    _  -> Left ("superfluous argument: " ++ show input)

  actionArguments _ _ = []

instance (Argument a, IsAction b c) => IsAction (a -> b) c where
  toAction action = Action $ \input -> argumentParser input >>= \(a, s) -> (unAction . toAction) (action a) s

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


-- | An argument parser.
type ArgumentParser a = String -> Either String (a, String)

-- | An argument.
class Argument a where
  argumentName   :: a -> String
  argumentParser :: ArgumentParser a

-- | A parser for arguments in the Read class.
readParser :: forall a . (Read a, Argument a) => ArgumentParser a
readParser = mkParser maybeRead

-- | A helper function for constructing argument parsers.
mkParser :: forall a . (Argument a) => (String -> Maybe a) -> ArgumentParser a
mkParser f input = case breakWord input of
  ("", _) -> Left missing
  (x, xs) -> maybe (Left $ err x) (\y -> Right (y, xs)) (f x)
  where
    name    = argumentName (undefined :: a)
    missing = "missing required argument: " ++ name
    err x   = "Argument '" ++ x ++ "' is not a valid " ++ name ++ "!"

-- | Break string at the next word boundary.
--
-- Any leading whitespace is stripped.
breakWord :: String -> (String, String)
breakWord = break isSpace . dropWhile isSpace

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
