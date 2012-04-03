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

parserFail :: String -> Parser a
parserFail = Parser . const . Left

takeWhile :: (Char -> Bool) -> Parser String
takeWhile p = Parser $ Right . span p

takeWhile1 :: (Char -> Bool) -> Parser String
takeWhile1 p = Parser go
  where
    go ""         = Left "takeWhile1: end of input"
    go (x:xs)
      | p x       = let (ys, zs) = span p xs in Right (x:ys, zs)
      | otherwise = Left ("takeWhile1: unexpected " ++ show x)

skipWhile :: (Char -> Bool) -> Parser ()
skipWhile p = takeWhile p *> pure ()
