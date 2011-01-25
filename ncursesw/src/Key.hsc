module Key where

import Data.Char (chr)

#include "mycurses.h"

------------------------------------------------------------------------
-- getch(3NCURSES)
------------------------------------------------------------------------

-- TODO KEY_F


-- | Break key
keyBreak :: Char
keyBreak  = chr (#const KEY_BREAK)

-- | The four arrow keys ...
keyDown :: Char
keyDown  = chr (#const KEY_DOWN)

keyUp :: Char
keyUp  = chr (#const KEY_UP)

keyLeft :: Char
keyLeft  = chr (#const KEY_LEFT)

keyRight :: Char
keyRight  = chr (#const KEY_RIGHT)

-- | Home key (upward+left arrow)
keyHome :: Char
keyHome  = chr (#const KEY_HOME)

-- | Backspace
keyBackspace :: Char
keyBackspace  = chr (#const KEY_BACKSPACE)

-- | Delete line
keyDl :: Char
keyDl  = chr (#const KEY_DL)

-- | Insert line
keyIl :: Char
keyIl  = chr (#const KEY_IL)

-- | Delete character
keyDc :: Char
keyDc  = chr (#const KEY_DC)

-- | Insert char or enter insert mode
keyIc :: Char
keyIc  = chr (#const KEY_IC)

-- | Exit insert char mode
keyEic :: Char
keyEic  = chr (#const KEY_EIC)

-- | Clear screen
keyClear :: Char
keyClear  = chr (#const KEY_CLEAR)

-- | Clear to end of screen
keyEos :: Char
keyEos  = chr (#const KEY_EOS)

-- | Clear to end of line
keyEol :: Char
keyEol  = chr (#const KEY_EOL)

-- | Scroll 1 line forward
keySf :: Char
keySf  = chr (#const KEY_SF)

-- | Scroll 1 line backward (reverse)
keySr :: Char
keySr  = chr (#const KEY_SR)

-- | Next page
keyNpage :: Char
keyNpage  = chr (#const KEY_NPAGE)

-- | Previous page
keyPpage :: Char
keyPpage  = chr (#const KEY_PPAGE)


-- | Clear tab
keyCtab :: Char
keyCtab  = chr (#const KEY_CTAB)

-- | Clear all tabs
keyCatab :: Char
keyCatab  = chr (#const KEY_CATAB)

-- | Enter or send
keyEnter :: Char
keyEnter  = chr (#const KEY_ENTER)

-- | Soft (partial) reset
keySreset :: Char
keySreset  = chr (#const KEY_SRESET)

-- | Reset or hard reset
keyReset :: Char
keyReset  = chr (#const KEY_RESET)

-- | Print or copy
keyPrint :: Char
keyPrint  = chr (#const KEY_PRINT)

-- | Home down or bottom (lower left)
keyLl :: Char
keyLl  = chr (#const KEY_LL)

-- | Upper left of keypad
keyA1 :: Char
keyA1  = chr (#const KEY_A1)

-- | Upper right of keypad
keyA3 :: Char
keyA3  = chr (#const KEY_A3)

-- | Center of keypad
keyB2 :: Char
keyB2  = chr (#const KEY_B2)

-- | Lower left of keypad
keyC1 :: Char
keyC1  = chr (#const KEY_C1)

-- | Lower right of keypad
keyC3 :: Char
keyC3  = chr (#const KEY_C3)

-- | Back tab key
keyBtab :: Char
keyBtab  = chr (#const KEY_BTAB)

-- | Beg(inning) key
keyBeg :: Char
keyBeg  = chr (#const KEY_BEG)

-- | Cancel key
keyCancel :: Char
keyCancel  = chr (#const KEY_CANCEL)

-- | Close key
keyClose :: Char
keyClose  = chr (#const KEY_CLOSE)

-- | Cmd (command) key
keyCommand :: Char
keyCommand  = chr (#const KEY_COMMAND)

-- | Copy key
keyCopy :: Char
keyCopy  = chr (#const KEY_COPY)

-- | Create key
keyCreate :: Char
keyCreate  = chr (#const KEY_CREATE)

-- | End key
keyEnd :: Char
keyEnd  = chr (#const KEY_END)

-- | Exit key
keyExit :: Char
keyExit  = chr (#const KEY_EXIT)

-- | Find key
keyFind :: Char
keyFind  = chr (#const KEY_FIND)

-- | Help key
keyHelp :: Char
keyHelp  = chr (#const KEY_HELP)

-- | Mark key
keyMark :: Char
keyMark  = chr (#const KEY_MARK)

-- | Message key
keyMessage :: Char
keyMessage  = chr (#const KEY_MESSAGE)

-- | Mouse event read
keyMouse :: Char
keyMouse  = chr (#const KEY_MOUSE)

-- | Move key
keyMove :: Char
keyMove  = chr (#const KEY_MOVE)

-- | Next object key
keyNext :: Char
keyNext  = chr (#const KEY_NEXT)

-- | Open key
keyOpen :: Char
keyOpen  = chr (#const KEY_OPEN)

-- | Options key
keyOptions :: Char
keyOptions  = chr (#const KEY_OPTIONS)

-- | Previous object key
keyPrevious :: Char
keyPrevious  = chr (#const KEY_PREVIOUS)

-- | Redo key
keyRedo :: Char
keyRedo  = chr (#const KEY_REDO)

-- | Ref(erence) key
keyReference :: Char
keyReference  = chr (#const KEY_REFERENCE)

-- | Refresh key
keyRefresh :: Char
keyRefresh  = chr (#const KEY_REFRESH)

-- | Replace key
keyReplace :: Char
keyReplace  = chr (#const KEY_REPLACE)

-- | Screen resized
keyResize :: Char
keyResize  = chr (#const KEY_RESIZE)

-- | Restart key
keyRestart :: Char
keyRestart  = chr (#const KEY_RESTART)

-- | Resume key
keyResume :: Char
keyResume  = chr (#const KEY_RESUME)

-- | Save key
keySave :: Char
keySave  = chr (#const KEY_SAVE)

-- | Shifted beginning key
keySbeg :: Char
keySbeg  = chr (#const KEY_SBEG)

-- | Shifted cancel key
keyScancel :: Char
keyScancel  = chr (#const KEY_SCANCEL)

-- | Shifted command key
keyScommand :: Char
keyScommand  = chr (#const KEY_SCOMMAND)

-- | Shifted copy key
keyScopy :: Char
keyScopy  = chr (#const KEY_SCOPY)

-- | Shifted create key
keyScreate :: Char
keyScreate  = chr (#const KEY_SCREATE)

-- | Shifted delete char key
keySdc :: Char
keySdc  = chr (#const KEY_SDC)

-- | Shifted delete line key
keySdl :: Char
keySdl  = chr (#const KEY_SDL)

-- | Select key
keySelect :: Char
keySelect  = chr (#const KEY_SELECT)

-- | Shifted end key
keySend :: Char
keySend  = chr (#const KEY_SEND)

-- | Shifted clear line key
keySeol :: Char
keySeol  = chr (#const KEY_SEOL)

-- | Shifted exit key
keySexit :: Char
keySexit  = chr (#const KEY_SEXIT)

-- | Shifted find key
keySfind :: Char
keySfind  = chr (#const KEY_SFIND)

-- | Shifted help key
keyShelp :: Char
keyShelp  = chr (#const KEY_SHELP)

-- | Shifted home key
keyShome :: Char
keyShome  = chr (#const KEY_SHOME)

-- | Shifted input key
keySic :: Char
keySic  = chr (#const KEY_SIC)

-- | Shifted left arrow key
keySleft :: Char
keySleft  = chr (#const KEY_SLEFT)

-- | Shifted message key
keySmessage :: Char
keySmessage  = chr (#const KEY_SMESSAGE)

-- | Shifted move key
keySmove :: Char
keySmove  = chr (#const KEY_SMOVE)

-- | Shifted next key
keySnext :: Char
keySnext  = chr (#const KEY_SNEXT)

-- | Shifted options key
keySoptions :: Char
keySoptions  = chr (#const KEY_SOPTIONS)

-- | Shifted prev key
keySprevious :: Char
keySprevious  = chr (#const KEY_SPREVIOUS)

-- | Shifted print key
keySprint :: Char
keySprint  = chr (#const KEY_SPRINT)

-- | Shifted redo key
keySredo :: Char
keySredo  = chr (#const KEY_SREDO)

-- | Shifted replace key
keySreplace :: Char
keySreplace  = chr (#const KEY_SREPLACE)

-- | Shifted right arrow
keySright :: Char
keySright  = chr (#const KEY_SRIGHT)

-- | Shifted resume key
keySrsume :: Char
keySrsume  = chr (#const KEY_SRSUME)


-- | Shifted save key
keySsave :: Char
keySsave  = chr (#const KEY_SSAVE)

-- | Shifted suspend key
keySsuspend :: Char
keySsuspend  = chr (#const KEY_SSUSPEND)

-- | Shifted undo key
keySundo :: Char
keySundo  = chr (#const KEY_SUNDO)

-- | Suspend key
keySuspend :: Char
keySuspend  = chr (#const KEY_SUSPEND)

-- | Undo key
keyUndo :: Char
keyUndo  = chr (#const KEY_UNDO)
