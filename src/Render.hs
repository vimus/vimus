{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Render (
  Render
, runRender
, getWindowSize
, addstr
, chgat
) where

import           Control.Monad.Reader
import           UI.Curses hiding (wgetch, ungetch, mvaddstr, err, mvwchgat, addstr)

import           Type
import           WindowLayout

data Environment = Environment {
  targetWindow :: Window
, drawingArea  :: WindowSize
}

newtype Render a = Render (ReaderT Environment IO a)
  deriving (Functor, Monad)

runRender :: Window -> WindowSize -> Render () -> IO ()
runRender window ws (Render action) = runReaderT action (Environment window ws)

getWindowSize :: Render WindowSize
getWindowSize = Render (asks drawingArea)

addstr :: Int -> Int -> String -> Render ()
addstr y x str = Render $ do
  WindowSize sizeY sizeX <- asks drawingArea
  when (y < sizeY) $ do
    window <- asks targetWindow
    let n = sizeX - x
    void $ liftIO $ mvwaddnstr window y x str n

chgat :: Int -> [Attribute] -> WindowColor -> Render ()
chgat y attr wc = Render $ do
  WindowSize sizeY _ <- asks drawingArea
  when (y < sizeY) $ do
    window <- asks targetWindow
    liftIO $ mvwchgat window y 0 (-1) attr wc
