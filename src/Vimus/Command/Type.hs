{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Vimus.Command.Type (
  module Vimus.Command.Type
, CompletionFunction
, noCompletion
) where

import           Control.Applicative

import           Vimus.Type
import           Vimus.Input (CompletionFunction, noCompletion)
import           Vimus.Command.Parser

newtype Action a = Action {unAction :: Parser a}
  deriving (Functor, Applicative, Alternative)

type VimusAction = Action (Vimus ())

newtype Help = Help {unHelp :: [String]}

data ArgumentInfo = ArgumentInfo {
  argumentInfoName   :: String
, argumentInfoComplete :: CompletionFunction
}

-- | A command.
data Command = Command {
  commandName        :: String
, commandHelp_       :: Help
, commandArguments   :: [ArgumentInfo]
, commandAction      :: VimusAction
}
