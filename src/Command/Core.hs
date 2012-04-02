module Command.Core where

import Vimus (Vimus)

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
command1 :: String -> (String -> Vimus ()) -> Command
command1 name action = Command name (Action1 action)

-- | Define a command that takes two arguments.
command2 :: String -> (String -> String -> Vimus ()) -> Command
command2 name action = Command name (Action2 action)

-- | Define a command that takes three arguments.
command3 :: String -> (String -> String -> String -> Vimus ()) -> Command
command3 name action = Command name (Action3 action)
