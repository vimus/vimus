{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module WindowLayout (
  Attribute (..)
, WindowColor (MainColor, RulerColor, TabColor, InputColor, PlayStatusColor, SongStatusColor, ErrorColor, SuggestionsColor)
, defaultColor
, create
, wchgat
, mvwchgat
, wcolor_set
, defineColor
) where

import           Control.Monad
import           UI.Curses hiding (wchgat, mvwchgat, wcolor_set)
import qualified UI.Curses as Curses

-- ReservedColor (color pair 0) cannot be modified, so we do not use it.
{-# WARNING ReservedColor "Do not use this!" #-}
data WindowColor =
    ReservedColor
  | MainColor
  | RulerColor
  | TabColor
  | InputColor
  | PlayStatusColor
  | SongStatusColor
  | ErrorColor
  | SuggestionsColor
  deriving (Show, Enum, Bounded)

defaultColor :: Color
defaultColor = Color (-1)

defineColor :: WindowColor -> Color -> Color -> IO ()
defineColor color fg bg = init_pair (fromEnum color) fg bg >> return ()

setWindowColor :: WindowColor -> Window -> IO Status
setWindowColor color window = wbkgd window (color_pair . fromEnum $ color)

wchgat :: Window -> Int -> [Attribute] -> WindowColor -> IO ()
wchgat window n attr color = void $ Curses.wchgat window n attr (fromEnum color)

mvwchgat :: Window -> Int -> Int -> Int -> [Attribute] -> WindowColor -> IO ()
mvwchgat window y x n attr color = void $ Curses.mvwchgat window y x n attr (fromEnum color)

wcolor_set :: Window -> WindowColor -> IO ()
wcolor_set window color = void $ Curses.wcolor_set window (fromEnum color)

-- | Set given color pair to default/default
resetColor :: WindowColor -> IO ()
resetColor c = defineColor c defaultColor defaultColor >> return ()

-- | Set all color pairs to default/default
resetColors :: IO ()
resetColors = mapM_ resetColor [toEnum 1 .. maxBound]

create :: IO (IO Window, Window, Window, Window, Window, Window)
create = do

  resetColors

  let createMainWindow = do
        (sizeY, _)    <- getmaxyx stdscr
        let mainWinSize = sizeY - 4
        window <- newwin mainWinSize 0 1 0
        setWindowColor MainColor window
        return (window, 0, mainWinSize + 1, mainWinSize + 2, mainWinSize + 3)

  (mainWindow, pos0, pos2, pos3, pos4) <- createMainWindow
  tabWindow        <- newwin 1 0 pos0 0
  songStatusWindow <- newwin 1 0 pos2 0
  playStatusWindow <- newwin 1 0 pos3 0
  inputWindow      <- newwin 1 0 pos4 0

  setWindowColor TabColor        tabWindow
  setWindowColor InputColor      inputWindow
  setWindowColor PlayStatusColor playStatusWindow
  setWindowColor SongStatusColor songStatusWindow

  let onResize = do
        (newMainWindow, newPos0, newPos2, newPos3, newPos4) <- createMainWindow
        mvwin tabWindow        newPos0 0
        mvwin songStatusWindow newPos2 0
        mvwin playStatusWindow newPos3 0
        mvwin inputWindow      newPos4 0
        wrefresh songStatusWindow
        wrefresh playStatusWindow
        wrefresh inputWindow
        return newMainWindow

  return (onResize, tabWindow, mainWindow, songStatusWindow, playStatusWindow, inputWindow)
