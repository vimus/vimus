{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, ScopedTypeVariables, CPP #-}
module Command.Core (
  Command (..)
, Argument (..)
, Action
, runAction
, command
, command0
, command1
, command2
, command3

#ifdef TEST
, argumentErrorMessage
#endif
) where

import           Vimus (Vimus, printError)
import           Util (maybeRead)
import           Control.Applicative
import qualified Data.Char as Char

import           WindowLayout (WindowColor(..), defaultColor)
import           UI.Curses (Color, black, red, green, yellow, blue, magenta, cyan, white)

-- | An action.
data Action =

  -- | An action that expects an arbitrary (possibly empty) strings as argument
  --
  -- This can be used to implement variadic actions.
    Action  (String -> Vimus ())

  -- | An action that expects no arguments
  | Action0 (Vimus ())

  -- | An action that expects one argument
  | Action1 (String -> Vimus ())

  -- | An action that expects two arguments
  | Action2 (String -> String -> Vimus ())

  -- | An action that expects three arguments
  | Action3 (String -> String -> String -> Vimus ())

runAction :: String -> Action -> Vimus ()
runAction s action =
  case action of
    Action  a -> a s
    Action0 a -> case args of
      [] -> a
      xs -> argumentError 0 xs
    Action1 a -> case args of
      [x] -> a x
      xs  -> argumentError 1 xs
    Action2 a -> case args of
      [x, y] -> a x y
      xs     -> argumentError 2 xs
    Action3 a -> case args of
      [x, y, z] -> a x y z
      xs        -> argumentError 3 xs
  where
    args = words s

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

-- | A command.
data Command = Command {
  commandName   :: String
, commandAction :: Action
}

-- | Define a command.
command :: String -> (String -> Vimus ()) -> Command
command name action = Command name (Action action)

-- | Define a command that takes no arguments.
command0 :: String -> Vimus () -> Command
command0 name action = Command name (Action0 action)

-- | Define a command that takes one argument.
command1 :: (Argument a) => String -> (a -> Vimus ()) -> Command
command1 name action = Command name . Action1 $ \x -> do
  either printError id $ action <$> parseArgument x

-- | Define a command that takes two arguments.
command2 :: (Argument a, Argument b) => String -> (a -> b -> Vimus ()) -> Command
command2 name action = Command name . Action2 $ \x y -> do
  either printError id $ action <$> parseArgument x <*> parseArgument y

-- | Define a command that takes three arguments.
command3 :: (Argument a, Argument b, Argument c) => String -> (a -> b -> c -> Vimus ()) -> Command
command3 name action = Command name . Action3 $ \x y z -> do
  either printError id $ action <$> parseArgument x <*> parseArgument y <*> parseArgument z

-- |
-- Minimal complete definition: `argumentName` and either `tryParseArgument` or
-- `parseArgument`.
class Argument a where

  argumentName  :: a -> String

  tryParseArgument :: String -> Maybe a
  tryParseArgument = either (const Nothing) Just . parseArgument

  parseArgument :: String -> Either String a
  parseArgument input = maybe (Left err) Right (tryParseArgument input)
    where
      err = "Argument '" ++ input ++ "' is not a valid " ++ name ++ "!"
      name = argumentName (undefined :: a)

instance Argument Int where
  argumentName     = const "int"
  tryParseArgument = maybeRead

instance Argument Integer where
  argumentName     = const "integer"
  tryParseArgument = maybeRead

instance Argument Float where
  argumentName     = const "float"
  tryParseArgument = maybeRead

instance Argument Double where
  argumentName     = const "double"
  tryParseArgument = maybeRead

instance Argument String where
  argumentName     = const "string"
  tryParseArgument = return

instance Argument WindowColor where
  argumentName           = const "color name"
  tryParseArgument input = case map Char.toLower input of
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
  argumentName           = const "color"
  tryParseArgument input = case map Char.toLower input of
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
