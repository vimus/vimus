{-# LANGUAGE FlexibleInstances #-}
module Widget where

import Control.Monad.Trans (MonadIO)

import UI.Curses (Window)

import ListWidget (ListWidget)
import qualified ListWidget

import WindowLayout (Colorable)

class Widget a where
  render :: (MonadIO m) => Window -> a -> m ()
  title  :: a -> String

instance (Show s, Colorable s) => Widget (ListWidget s) where
  render = ListWidget.render

  title l = case ListWidget.getParent l of
    Just p  -> ListWidget.breadcrumbs p
    Nothing -> ""
