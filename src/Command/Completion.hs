module Command.Completion (
  completeCommand
) where

import           Data.List

import           Util
import           Input (CompletionFunction)
import           Command.Type

completeCommand :: [Command] -> CompletionFunction
completeCommand = autoComplete_ . map commandName

autoComplete_ :: [String] -> CompletionFunction
autoComplete_ names input = case filter (isPrefixOf input) names of
  [x] -> Right (x ++ " ")
  xs  -> case commonPrefix $ map (drop $ length input) xs of
    "" -> Left xs
    ys -> Right (input ++ ys)
