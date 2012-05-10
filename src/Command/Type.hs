{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Command.Type where

import           Control.Applicative

import           Vimus
import           Command.Parser

newtype Action a = Action {unAction :: Parser a}
  deriving (Functor, Applicative, Alternative)

type VimusAction = Action (Vimus ())

-- | A command.
data Command = Command {
  commandName        :: String
, commandDescription :: String
, commandArguments   :: [String]
, commandAction      :: VimusAction
}
