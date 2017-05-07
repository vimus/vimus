-- | Song format parser
module Vimus.Song.Format (
  SongFormat(..)
, parser

-- * exported for testing
, FormatTree(..)
, format
, meta
, alternatives
, parse
) where

import           Control.Applicative (Alternative(..), pure, liftA2)
import           Data.Default (Default(..))
import           Data.Foldable (asum)
import           Data.List (intercalate)
import           Data.Maybe (fromMaybe)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Monoid (Monoid(..))
import           Text.Printf (printf)

import qualified Network.MPD as MPD

import qualified Vimus.Command.Parser as Parser

import Vimus.Song


infixr 4 :+:

-- | AST for formats:
data FormatTree s m a =
    Empty
  | Pure a
  | FormatTree s m a :+: FormatTree s m a
  | Meta (s -> m a)
  | Alt [FormatTree s m a]

-- | Format AST
format
    :: (Alternative m, Monoid a)
    => a -- ^ default value for failed top-level metadata query
    -> s -- ^ container for metadata
    -> FormatTree s m a -> m a
format d n = top where
  top Empty     = empty
  top (Pure a)  = pure a
  top (x :+: y) = top x <#> top y
  top (Alt xs)  = asum $ fmap nested xs
  top (Meta f)  = f n <|> pure d -- if metadata query failed, replace failure with 'd'

  nested (Meta f)  = f n
  nested (x :+: y) = nested x <#> nested y
  nested t         = top t

  (<#>) = liftA2 mappend


newtype SongFormat = SongFormat (MPD.Song -> String)

instance Default SongFormat where
  def = SongFormat $ \song ->
    printf "%s - %s - %s - %s"
      (orNone $ artist song)
      (orNone $ album song)
      (orNone $ track song)
      (orNone $ title song <|> filename song)
   where
    orNone = fromMaybe "(none)"

parser :: Map String (MPD.Song -> Maybe String) -> Parser.Parser SongFormat
parser queries = Parser.Parser $ \str -> do
  tree <- parse queries str
  return (SongFormat (\song -> fromMaybe "(none)" $ format "none" song tree), "")

parse
  :: Map String (MPD.Song -> Maybe String)
  -> String
  -> Either Parser.ParseError (FormatTree MPD.Song Maybe String)
parse queries = go "" where
  go acc ('\\':'(':cs) = go ('(':acc) cs
  go acc ('\\':')':cs) = go (')':acc) cs
  go acc ('\\':'%':cs) = go ('%':acc) cs
  go acc ('(':cs) = do
    (xs, ys) <- alternatives cs
    alts     <- mapM (go "") xs
    rest     <- go "" ys
    return $ Pure (reverse acc) <+> Alt alts <+> rest
  go acc ('%':cs) = do
    (key, ys) <- meta cs
    case Map.lookup key queries of
      Nothing -> Left (Parser.ParseError $ "non-supported meta pattern: %" ++ key ++ "%")
      Just metadata -> do
        rest <- go "" ys
        return $ Pure (reverse acc) <+> Meta metadata <+> rest
  go acc (c:cs) = go (c:acc) cs
  go acc [] = Right (Pure (reverse acc))

  infixr 4 <+>
  Pure "" <+> x = x
  x <+> Pure "" = x
  x <+>       y = x :+: y


data Nat = Z | S Nat

-- | Parse alternatives pattern
alternatives :: String -> Either Parser.ParseError ([String], String)
alternatives = go Z [] "" where
  go n strings acc ('\\':'(':xs) = go n strings ('(':'\\':acc) xs
  go n strings acc ('\\':')':xs) = go n strings (')':'\\':acc) xs
  go n strings acc ('\\':'|':xs) = go n strings ('|':'\\':acc) xs
  go n strings acc ('(':xs)      = go (S n) strings ('(':acc) xs
  go Z strings acc (')':xs)      = Right (reverse (reverse acc : strings), xs)
  go (S n) strings acc (')':xs)  = go n strings (')':acc) xs
  go Z strings acc ('|':xs)      = go Z (reverse acc : strings) [] xs
  go n strings acc (x:xs)        = go n strings (x:acc) xs
  go _ strings acc []            = Left . Parser.ParseError $
    "unterminated alternatives pattern: (" ++ intercalate "|" (reverse acc : strings)

-- | Parse meta pattern
meta :: String -> Either Parser.ParseError (String, String)
meta = go "" where
  go acc ('\\':'%':xs) = go ('%':acc) xs
  go acc ('%':xs)      = Right (reverse acc, xs)
  go acc (x:xs)        = go (x:acc) xs
  go acc []            = Left (Parser.ParseError $ "unterminated meta pattern: %" ++ reverse acc)


