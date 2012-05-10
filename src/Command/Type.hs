{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Command.Type where

import           Control.Applicative

import           Vimus
import           Command.Parser

newtype Action a = Action {unAction :: Parser a}
  deriving (Functor, Applicative, Alternative)

type VimusAction = Action (Vimus ())

newtype Help = Help {unHelp :: [String]}

-- | A command.
data Command = Command {
  commandName        :: String
, commandHelp_       :: Help
, commandArguments   :: [String]
, commandAction      :: VimusAction
}
