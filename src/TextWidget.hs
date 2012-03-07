{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module TextWidget (
  TextWidget
, new
) where

import           Control.Monad.Trans (liftIO)

import           UI.Curses hiding (wgetch, ungetch, mvaddstr)

import           Widget

data TextWidget = TextWidget {
  text :: [String]
}

new :: [String] -> TextWidget
new t = TextWidget t

instance Widget TextWidget where
  render window widget = liftIO $ do
    werase window
    mvwaddstr window 0 0 $ unlines $ text widget
    wrefresh window
    return ()

  title = unwords . text
