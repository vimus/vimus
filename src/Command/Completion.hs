module Command.Completion (
  completeCommand
, parseCommand
, completeOptions
) where

import           Data.List
import           Data.Char

import           Util
import           Input (CompletionFunction)
import           Command.Type

completeCommand :: [Command] -> CompletionFunction
completeCommand commands input_ = (pre ++) `fmap` case parseCommand_ input of
  (c, "")   -> completeCommandName c
  (c, args) -> case filter ((== c) . commandName) commands of
    -- TODO: argument completion for all commands in the list
    x:_ -> (c ++) `fmap` completeArguments (commandArguments x) args
    []  -> Left []
  where
    (pre, input) = span isSpace input_

    -- a completion function for command names
    completeCommandName :: CompletionFunction
    completeCommandName = completeOptions (map commandName commands)

completeArguments :: [ArgumentInfo] -> CompletionFunction
completeArguments = go
  where
    go specs_ input_ = (pre ++) `fmap` case specs_ of
      [] -> Left []
      spec:specs -> case break isSpace input of
        (arg, "")   -> argumentInfoComplete spec arg
        (arg, args) -> (arg ++) `fmap` go specs args
      where
        (pre, input) = span isSpace input_

-- | Create a completion function from a list of possibilities.
completeOptions :: [String] -> CompletionFunction
completeOptions options input = case filter (isPrefixOf input) options of
  [x] -> Right (x ++ " ")
  xs  -> case commonPrefix $ map (drop $ length input) xs of
    "" -> Left xs
    ys -> Right (input ++ ys)


-- | Split given input into a command name and a rest (the arguments).
--
-- Whitespace in front of the command name and the arguments is striped.
parseCommand :: String -> (String, String)
parseCommand input = (name, dropWhile isSpace args)
  where (name, args) = parseCommand_ (dropWhile isSpace input)

-- | Like `parseCommand`, but assume that input starts with a non-whitespace
-- character, and retain whitespace in front of the arguments.
parseCommand_ :: String -> (String, String)
parseCommand_ input = (name, args)
  where
    (name, args) = case input of
      '!':xs -> ("!", xs)
      xs     -> break isSpace xs
