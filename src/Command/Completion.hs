module Command.Completion (
  completeCommand
) where

import           Data.List
import           Data.Char

import           Util
import           Input (CompletionFunction)
import           Command.Type

completeCommand :: [Command] -> CompletionFunction
completeCommand commands input_ = (pre ++) `fmap` case parseCommand input of
  (c, "")   -> completeCommandName c
  (c, args) -> case filter ((== c) . commandName) commands of
    -- TODO: argument completion for all commands in the list
    x:_ -> (c ++) `fmap` completeArguments (commandArguments x) args
    []  -> Left []
  where
    (pre, input) = span isSpace input_

    -- a completion function for command names
    completeCommandName :: CompletionFunction
    completeCommandName = complete (map commandName commands)

completeArguments :: [ArgumentSpec] -> CompletionFunction
completeArguments = go
  where
    go specs_ input_ = (pre ++) `fmap` case specs_ of
      [] -> Left []
      spec:specs -> case break isSpace input of
        (arg, "")   -> complete (argumentSpecValues spec) arg
        (arg, args) -> (arg ++) `fmap` go specs args
      where
        (pre, input) = span isSpace input_

-- | Create a completion function from a list of possibilities.
complete :: [String] -> CompletionFunction
complete options input = case filter (isPrefixOf input) options of
  [x] -> Right (x ++ " ")
  xs  -> case commonPrefix $ map (drop $ length input) xs of
    "" -> Left xs
    ys -> Right (input ++ ys)

-- FIXME: modified version of Command.parseCommand
parseCommand :: String -> (String, String)
parseCommand input = (c, args)
  where
    (c, args) = case input of
      '!':xs -> ("!", xs)
      xs     -> break isSpace xs
