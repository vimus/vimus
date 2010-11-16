{-# OPTIONS_GHC -XTemplateHaskell #-}
module ListWidgetTest where

import Test.Framework.TH
import Test.Framework.Providers.QuickCheck2

import ListWidget

main = $(defaultMainGenerator)

prop_confine lower upper n_
  | upper <= lower  = n == lower
  | otherwise       = lower <= n && n < upper
  where
    n = confine lower upper n_
