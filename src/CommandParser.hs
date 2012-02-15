{-# LANGUAGE OverloadedStrings #-}
module CommandParser (parseMappingArg) where

import           Prelude hiding (takeWhile)
import           Control.Applicative
import           Data.Attoparsec.Text
import           Control.Monad
import qualified Data.Text as Text
import           Data.Text   (Text)


parseMappingArg :: String -> Maybe String
parseMappingArg = parse_ (mappingArg <|> pure "")

parse_ :: Parser a -> String -> Maybe a
parse_ p input = toMaybe $ parseOnly (p <* endOfInput) (Text.pack input)
  where
    toMaybe = either (const Nothing) Just

mappingArg :: Parser String
mappingArg = Text.unpack . Text.concat <$> many1 (plain <|> key) <?> "foobar"

plain :: Parser Text
plain = takeWhile1 (/= '<')

key :: Parser Text
key = p >>= \k ->
  case Text.toUpper k of
    "CR" -> return "\n"
    _    -> mzero
  where
    p = char '<' *> takeWhile (/='>') <* char '>'
