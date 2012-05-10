{-# LANGUAGE TemplateHaskell #-}
module Command.Help (
  Help (..)
, help
) where

import           Control.Applicative
import           Data.String
import           Language.Haskell.TH
import           Language.Haskell.TH.Quote
import           Language.Haskell.TH.Syntax

import           Util (strip)

-- | A `QuasiQuoter` for help text.
help :: QuasiQuoter
help = QuasiQuoter {
    quoteExp  = lift . parseHelp
  , quotePat  = error "Command.Help.help: quotePat is undefined!"
  , quoteType = error "Command.Help.help: quoteType is undefined!"
  , quoteDec  = error "Command.Help.help: quoteDec is undefined!"
  }

newtype Help = Help {unHelp :: [String]}

instance Lift Help where
  lift (Help xs) = AppE <$> [|Help|] <*> lift xs

instance IsString Help where
  fromString = parseHelp

-- | Parse help text.
parseHelp :: String -> Help
parseHelp = Help . go . map strip . lines
  where
    go l = case dropWhile null l of
      [] -> []
      xs -> let (ys, zs) = break null xs in (wordWrapping . unwords) ys ++ go zs

-- | Apply automatic word wrapping.
wordWrapping :: String -> [String]
wordWrapping = run . words
  where
    -- we start each recursion step at 2, this makes sure that each step
    -- consumes at least one word
    run = go 2
    go n xs
      | null xs                  = []
      | 60 < length (unwords ys) = let (as, bs) = splitAt (pred n) xs in unwords as : run bs
      | null zs                  = [unwords ys]
      | otherwise                = go (succ n) xs
      where
        (ys, zs) = splitAt n xs
