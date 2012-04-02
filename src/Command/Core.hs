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

runAction :: String -> Action -> Vimus ()
runAction s action = either printError id (action s)

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

type Action = String -> Either String (Vimus ())
type Action_ = [String] -> Either String (Vimus ())

class IsAction a where
  toAction :: a -> Action_

instance IsAction (Vimus ()) where
  toAction action [] = Right action
  toAction _      s  = Left ("superfluous argument: " ++ show s)

instance (Argument a, IsAction b) => IsAction (a -> b) where
  toAction action (x:xs) = parseArgument x >>= (($ xs) . toAction <$> action)
  toAction _         []  = Left ("missing required argument: " ++ argumentName (undefined :: a))

-- | A command.
data Command = Command {
  commandName   :: String
, commandAction :: Action
}

-- | Define a command.
command :: IsAction a => String -> a -> Command
command name action = Command name (toAction action . words)

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
