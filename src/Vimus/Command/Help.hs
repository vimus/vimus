{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Vimus.Command.Help (
  Help (..)
, help
, commandShortHelp
, commandHelpText
) where

import           Control.Monad
import           Data.Maybe
import           Data.String
import           Language.Haskell.TH
import           Language.Haskell.TH.Quote
import           Language.Haskell.TH.Syntax

import           Vimus.Command.Type
import           Vimus.Util (strip)

-- | A `QuasiQuoter` for help text.
help :: QuasiQuoter
help = QuasiQuoter {
    quoteExp  = lift . parseHelp
  , quotePat  = error "Command.Help.help: quotePat is undefined!"
  , quoteType = error "Command.Help.help: quoteType is undefined!"
  , quoteDec  = error "Command.Help.help: quoteDec is undefined!"
  }

instance Lift Help where
  lift (Help xs) = AppE `fmap` [|Help|] `ap` lift xs

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

-- | The first line of the command description.
commandShortHelp :: Command -> String
commandShortHelp = fromMaybe "" . listToMaybe . unHelp . commandHelp_

commandHelpText :: Command -> [String]
commandHelpText = unHelp . commandHelp_
