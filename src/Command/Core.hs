{-# LANGUAGE CPP, TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses, FlexibleContexts, ScopedTypeVariables #-}
module Command.Core (
  Command
, commandName
, commandAction
, commandSynopsis
, Argument (..)
, ArgumentSpec (..)
, argumentParser
, Action (..)
, VimusAction
, runAction
, command

-- * Helpers for defining @Argument@ instances
, missingArgument
, invalidArgument
, specificArgumentError

#ifdef TEST
, readParser
, IsAction (..)
#endif
) where

import           Control.Applicative
import           Control.Monad (unless)
import           Data.Char

import           Vimus (Vimus)
import           Util (maybeRead)
import           WindowLayout (WindowColor(..), defaultColor)
import           UI.Curses (Color, black, red, green, yellow, blue, magenta, cyan, white)
import           Command.Type
import           Command.Help () -- for the (IsString Help) instance
import           Command.Parser

runAction :: Action a -> String -> Either String a
runAction action s = either (Left . show) (Right . fst) $ runParser (unAction action <* endOfInput) s


class IsAction a b where
  toAction :: a -> Action b
  actionArguments :: a -> b -> [ArgumentInfo]

instance IsAction a a where

  toAction a = Action $ do
    r <- takeInput
    unless (null r) $ do
      parserFail (SuperfluousInput r)
    return a

  actionArguments _ _ = []

instance (Argument a, IsAction b c) => IsAction (a -> b) c where
  toAction action = Action $ (argumentParser <* skipWhile isSpace) >>= unAction . toAction . action
  actionArguments _ _ = mkArgumentInfo (argumentSpec :: (ArgumentSpec a)) : actionArguments (undefined :: b) (undefined :: c)

-- | Get help text for given command.
commandSynopsis :: Command -> String
commandSynopsis c = unwords $ commandName c : map (\x -> "{" ++ argumentInfoName x ++ "}") (commandArguments c)

-- | Define a command.
command :: forall a . IsAction a (Vimus ()) => String -> Help -> a -> Command
command name description action = Command name description (actionArguments action (undefined :: Vimus ())) (toAction action)

-- | Create an ArgumentInfo from given ArgumentSpec.
mkArgumentInfo :: ArgumentSpec a -> ArgumentInfo
mkArgumentInfo arg = ArgumentInfo {
    argumentInfoName   = argumentSpecName arg
  , argumentInfoValues = argumentSpecValues arg
  }

-- | Like ArgumentInfo, but includes a parser for the argument.
data ArgumentSpec a = ArgumentSpec {
  argumentSpecName   :: String
, argumentSpecValues :: [String]
, argumentSpecParser :: Parser a
}

-- | An argument.
class Argument a where
  -- | A parser for this argument, together with a description.
  --
  -- The description provides information about the argument, that can be used
  -- for command-line completion and online help.
  --
  -- The parser can assume that the input is either empty or starts with a
  -- non-whitespace character.
  argumentSpec :: ArgumentSpec a


argumentParser :: Argument a => Parser a
argumentParser = argumentSpecParser argumentSpec

argumentName :: forall a . Argument a => a -> String
argumentName _ = argumentSpecName (argumentSpec :: ArgumentSpec a)

-- | A parser for arguments in the Read class.
readParser :: forall a . (Read a, Argument a) => Parser a
readParser = mkParser maybeRead

-- | A helper function for constructing argument parsers.
mkParser :: forall a . (Argument a) => (String -> Maybe a) -> Parser a
mkParser f = do
  r <- takeWhile1 (not . isSpace) <|> missingArgument (undefined :: a)
  maybe (invalidArgument (undefined ::a) r) return (f r)

-- | A failing parser that indicates a missing argument.
missingArgument :: Argument a => a -> Parser b
missingArgument = parserFail . MissingArgument . argumentName

-- | A failing parser that indicates an invalid argument.
invalidArgument :: Argument a => a -> Value -> Parser b
invalidArgument t = parserFail . InvalidArgument (argumentName t)

-- | A failing parser that indicates a specific error.  It takes precedence
-- over any other kind of error.
specificArgumentError :: String -> Parser b
specificArgumentError = parserFail . SpecificArgumentError

instance Argument Int where
  argumentSpec = ArgumentSpec "int" [] readParser

instance Argument Integer where
  argumentSpec = ArgumentSpec "integer" [] readParser

instance Argument Float where
  argumentSpec = ArgumentSpec "float" [] readParser

instance Argument Double where
  argumentSpec = ArgumentSpec "double" [] readParser

instance Argument String where
  argumentSpec = ArgumentSpec "string" [] (mkParser Just)

-- | Create an ArgumentSpec from an association list.
mkArgumentSpec :: Argument a => String -> [(String, a)] -> ArgumentSpec a
mkArgumentSpec name values = ArgumentSpec name (map fst values) parser
  where parser = mkParser ((`lookup` values) . map toLower)

instance Argument WindowColor where
  argumentSpec = mkArgumentSpec "item" [
      ("main", MainColor)
    , ("ruler", RulerColor)
    , ("tab", TabColor)
    , ("input", InputColor)
    , ("playstatus", PlayStatusColor)
    , ("songstatus", SongStatusColor)
    , ("error", ErrorColor)
    , ("suggestions", SuggestionsColor)
    ]

instance Argument Color where
  argumentSpec = mkArgumentSpec "color" [
      ("default", defaultColor)
    , ("black", black)
    , ("red", red)
    , ("green", green)
    , ("yellow", yellow)
    , ("blue", blue)
    , ("magenta", magenta)
    , ("cyan", cyan)
    , ("white", white)
    ]
