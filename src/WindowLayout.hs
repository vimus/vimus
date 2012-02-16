{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module WindowLayout (create) where

import UI.Curses


create :: IO (IO Window, Window, Window, Window, Window, Window, Window)
create = do

  -- define colors
  init_pair 1 green black
  init_pair 2 blue white
  init_pair 3 black white

  let createMainWindow = do
      (sizeY, _)    <- getmaxyx stdscr
      let mainWinSize = sizeY - 5
      window <- newwin mainWinSize 0 1 0
      wbkgd window $ color_pair 2
      wrefresh window
      return (window, 0, mainWinSize + 1, mainWinSize + 2, mainWinSize + 3, mainWinSize + 4)

  (mainWindow, pos0, pos1, pos2, pos3, pos4) <- createMainWindow
  tabWindow        <- newwin 1 0 pos0 0
  statusWindow     <- newwin 1 0 pos1 0
  songStatusWindow <- newwin 1 0 pos2 0
  playStatusWindow <- newwin 1 0 pos3 0
  inputWindow      <- newwin 1 0 pos4 0

  wbkgd tabWindow $ color_pair 1
  wrefresh tabWindow

  wbkgd inputWindow $ color_pair 1
  wrefresh inputWindow

  wbkgd statusWindow $ color_pair 1
  wrefresh statusWindow

  wbkgd playStatusWindow $ color_pair 3
  wrefresh playStatusWindow

  wbkgd songStatusWindow $ color_pair 3
  wrefresh songStatusWindow

  let onResize = do
      (newMainWindow, newPos0, newPos1, newPos2, newPos3, newPos4) <- createMainWindow
      mvwin tabWindow        newPos0 0
      mvwin statusWindow     newPos1 0
      mvwin songStatusWindow newPos2 0
      mvwin playStatusWindow newPos3 0
      mvwin inputWindow      newPos4 0
      wrefresh statusWindow
      wrefresh songStatusWindow
      wrefresh playStatusWindow
      wrefresh inputWindow
      return newMainWindow

  return (onResize, tabWindow, mainWindow, statusWindow, songStatusWindow, playStatusWindow, inputWindow)
