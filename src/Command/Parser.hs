{-# LANGUAGE DeriveFunctor #-}
module Command.Parser where

import           Prelude hiding (takeWhile)
import           Control.Monad
import           Control.Applicative
import           Control.Monad.Error ()

newtype Parser a = Parser {runParser :: String -> Either String (a, String)}
  deriving Functor

instance Applicative Parser where
  pure  = return
  (<*>) = ap

instance Monad Parser where
  fail      = parserFail
  return a  = Parser $ \input -> Right (a, input)
  p1 >>= p2 = Parser $ \input -> runParser p1 input >>= uncurry (runParser . p2)

instance Alternative Parser where
  empty = parserFail "empty"
  p1 <|> p2 = Parser $ \input -> case runParser p1 input of
    Left _ -> runParser p2 input
    x      -> x

-- | Recognize a character that satisfies a given predicate.
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser go
  where
    go (x:xs)
      | p x       = Right (x, xs)
      | otherwise = Left ("satisfy: unexpected " ++ show x)
    go ""         = Left "satisfy: unexpected end of input"

-- | Recognize a given character.
char :: Char -> Parser Char
char c = satisfy (== c)

-- | Recognize a given string.
string :: String -> Parser String
string = mapM char

parserFail :: String -> Parser a
parserFail = Parser . const . Left

takeWhile :: (Char -> Bool) -> Parser String
takeWhile p = Parser $ Right . span p

takeWhile1 :: (Char -> Bool) -> Parser String
takeWhile1 p = Parser go
  where
    go ""         = Left "takeWhile1: unexpected end of input"
    go (x:xs)
      | p x       = let (ys, zs) = span p xs in Right (x:ys, zs)
      | otherwise = Left ("takeWhile1: unexpected " ++ show x)

skipWhile :: (Char -> Bool) -> Parser ()
skipWhile p = takeWhile p *> pure ()

-- | Consume and return all remaining input.
takeInput :: Parser String
takeInput = Parser $ \input -> Right (input, "")

-- | Succeed only if all input has been consumed.
endOfInput :: Parser ()
endOfInput = Parser $ \input -> case input of
  "" -> Right ((), "")
  xs -> Left ("endOfInput: remaining input " ++ show xs)
