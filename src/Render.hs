{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Render (
  Render
, runRender
, getWindowSize
, addstr
, chgat
, withColor

-- exported to silence warnings
, Environment (..)
) where

import           Control.Applicative
import           Control.Monad.Reader
import           UI.Curses hiding (wgetch, ungetch, mvaddstr, err, mvwchgat, addstr, wcolor_set)

import           Type
import           WindowLayout

data Environment = Environment {
  environmentWindow  :: Window
, environmentOffsetY :: Int
, environmentOffsetX :: Int
, environmentSize    :: WindowSize
}

newtype Render a = Render (ReaderT Environment IO a)
  deriving (Functor, Monad, Applicative)

runRender :: Window -> Int -> Int -> WindowSize -> Render a -> IO a
runRender window y x ws (Render action) = runReaderT action (Environment window y x ws)

getWindowSize :: Render WindowSize
getWindowSize = Render (asks environmentSize)

-- | Translate given coordinates and run given action
--
-- The action is only run, if coordinates are within the drawing area.
withTranslated :: Int -> Int -> (Window -> Int -> Int -> Int -> IO a) -> Render ()
withTranslated y_ x_ action = Render $ do
  r <- ask
  case r of
    Environment window offsetY offsetX (WindowSize sizeY sizeX)
      |    0 <= x && x < (sizeX + offsetX)
        && 0 <= y && y < (sizeY + offsetY) -> liftIO $ void (action window y x n)
      | otherwise                          -> return ()
      where
        x = x_ + offsetX
        y = y_ + offsetY
        n = sizeX - x

addstr :: Int -> Int -> String -> Render ()
addstr y_ x_ str = withTranslated y_ x_ $ \window y x n ->
  mvwaddnstr window y x str n

chgat :: Int -> [Attribute] -> WindowColor -> Render ()
chgat y_ attr wc = withTranslated y_ 0 $ \window y x n ->
  mvwchgat window y x n attr wc

withColor :: WindowColor -> Render a -> Render a
withColor color action = do
  window <- Render $ asks environmentWindow
  setColor window color *> action <* setColor window MainColor
  where
    setColor w c = Render . liftIO $ wcolor_set w c
