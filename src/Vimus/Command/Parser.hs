{-# LANGUAGE DeriveFunctor #-}
module Vimus.Command.Parser where

import           Prelude hiding (takeWhile)
import           Data.List (intercalate)
import           Control.Monad
import           Control.Applicative

type Name  = String
type Value = String

-- | Errors are ordered from less specific to more specific.  More specific
-- errors take precedence over less specific ones.
data ParseError =
    Empty
  | ParseError String
  | SuperfluousInput Value
  | MissingArgument Name
  | InvalidArgument Name Value
  | SpecificArgumentError String
  deriving (Eq, Ord)

instance Show ParseError where
  show e = case e of
    Empty                      -> "Control.Applicative.Alternative.empty"
    ParseError err             -> "parse error: " ++ err
    SuperfluousInput input     -> case words input of
      []  -> "superfluous input: " ++ show input
      [x] -> "unexpected argument: " ++ show x
      xs  -> "unexpected arguments: " ++ intercalate ", " (map show xs)
    MissingArgument name       -> "missing required argument: " ++ name
    InvalidArgument name value -> "argument " ++ show value ++ " is not a valid " ++ name
    SpecificArgumentError err  -> err

newtype Parser a = Parser {runParser :: String -> Either ParseError (a, String)}
  deriving Functor

instance Applicative Parser where
  pure  = return
  (<*>) = ap

instance Monad Parser where
  return a  = Parser $ \input -> Right (a, input)
  p1 >>= p2 = Parser $ \input -> runParser p1 input >>= uncurry (runParser . p2)

instance MonadFail Parser where
  fail      = parserFail . ParseError

instance Alternative Parser where
  empty = parserFail Empty
  p1 <|> p2 = Parser $ \input -> case runParser p1 input of
    Left err -> either (Left . max err) (Right) (runParser p2 input)
    x        -> x

-- | Recognize a character that satisfies a given predicate.
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser go
  where
    go (x:xs)
      | p x       = Right (x, xs)
      | otherwise = (Left . ParseError) ("satisfy: unexpected " ++ show x)
    go ""         = (Left . ParseError) "satisfy: unexpected end of input"

-- | Recognize a given character.
char :: Char -> Parser Char
char c = satisfy (== c)

-- | Recognize a given string.
string :: String -> Parser String
string = mapM char

parserFail :: ParseError -> Parser a
parserFail = Parser . const . Left

takeWhile :: (Char -> Bool) -> Parser String
takeWhile p = Parser $ Right . span p

takeWhile1 :: (Char -> Bool) -> Parser String
takeWhile1 p = Parser go
  where
    go ""         = (Left . ParseError) "takeWhile1: unexpected end of input"
    go (x:xs)
      | p x       = let (ys, zs) = span p xs in Right (x:ys, zs)
      | otherwise = (Left . ParseError) ("takeWhile1: unexpected " ++ show x)

skipWhile :: (Char -> Bool) -> Parser ()
skipWhile p = takeWhile p *> pure ()

-- | Consume and return all remaining input.
takeInput :: Parser String
takeInput = Parser $ \input -> Right (input, "")

-- | Succeed only if all input has been consumed.
endOfInput :: Parser ()
endOfInput = Parser $ \input -> case input of
  "" -> Right ((), "")
  xs -> (Left . ParseError) ("endOfInput: remaining input " ++ show xs)
