{-# LANGUAGE ForeignFunctionInterface #-}

module Misc where

import Foreign.C.Types
import Foreign.Ptr
import Foreign.C.String
import Foreign.Marshal.Alloc
import Foreign.Storable
import Foreign.ForeignPtr

import Data.Char
import Data.List (foldl')
import Foreign.Marshal.Utils (fromBool, toBool)

import Data.Bits

import qualified Constant
import CursesUtil

{# import Type #}

-- import Foreign.Storable

#include "mycurses.h"



cFromBool = fromBool
cToBool   = toBool

------------------------------------------------------------------------
-- attr
------------------------------------------------------------------------

-- | Deprecated, use Attribute instead
newtype Attr = Attr CInt

(&) :: Attr -> Attr -> Attr
(Attr a) & (Attr b) = Attr (a .|. b)

fromAttr (Attr a) = a

-- {#fun unsafe attron  {fromAttr `Attr'} -> `Status' toStatus*#}
-- {#fun unsafe attroff {fromAttr `Attr'} -> `Status' toStatus*#}


{#enum define Attribute {
  WA_NORMAL     as Normal
, WA_STANDOUT   as Standout
, WA_UNDERLINE  as Underline
, WA_REVERSE    as Reverse
, WA_BLINK      as Blink
, WA_DIM        as Dim
, WA_BOLD       as Bold
, WA_ALTCHARSET as Altcharset
, WA_INVIS      as Invis
, WA_PROTECT    as Protect
}#}
-- not yet implemented by ncurses, so we do not define them
{-
, WA_HORIZONTAL as Horizontal
, WA_LEFT as Left
, WA_LOW as Low
, WA_RIGHT as Right
, WA_TOP as Top
, WA_VERTICAL as Vertical
-}


combine :: [Attribute] -> Attr_t
combine l = foldl' (.|.) 0 $ map (fromIntegral . fromEnum) l

-- int wcolor_set(WINDOW *win, short color_pair_number, void* opts);
--
-- int wstandend(WINDOW *win);
-- int wstandout(WINDOW *win);
--
-- int wattr_get(WINDOW *win, attr_t *attrs, short *pair, void *opts);

-- int wattr_off(WINDOW *win, attr_t attrs, void *opts);
{#fun unsafe wattr_off as wattr_off_ {id `Window', combine `[Attribute]', id `Ptr ()'} -> `Status' toStatus*#}
wattr_off win attrs = wattr_off_ win attrs nullPtr

-- int wattr_on(WINDOW *win, attr_t attrs, void *opts);
{#fun unsafe wattr_on as wattr_on_ {id `Window', combine `[Attribute]', id `Ptr ()'} -> `Status' toStatus*#}
wattr_on win attrs = wattr_on_ win attrs nullPtr

-- int wattr_set(WINDOW *win, attr_t attrs, short pair, void *opts);
--


-- int wchgat(WINDOW *win, int n, attr_t attr, short color, const void *opts)
{#fun unsafe wchgat as wchgat_ {id `Window', `Int', combine `[Attribute]', shortFromInt `Int', id `Ptr ()'} -> `Status' toStatus*#}
wchgat win n attrs color = wchgat_ win n attrs color nullPtr

-- int mvwchgat(WINDOW *win, int y, int x, int n, attr_t attr, short color, const void *opts)
{#fun unsafe mvwchgat as mvwchgat_ {id `Window', `Int', `Int', `Int', combine `[Attribute]', shortFromInt `Int', id `Ptr ()'} -> `Status' toStatus*#}

mvwchgat win y x n attrs color = mvwchgat_ win y x n attrs color nullPtr


------------------------------------------------------------------------
-- color(3NCURSES)
------------------------------------------------------------------------
newtype Color = Color CShort

fromColor :: Color -> CShort
fromColor (Color a) = a

black, red, green, yellow, blue, magenta, cyan, white :: Color
black   = Color Constant.black
red     = Color Constant.red
green   = Color Constant.green
yellow  = Color Constant.yellow
blue    = Color Constant.blue
magenta = Color Constant.magenta
cyan    = Color Constant.cyan
white   = Color Constant.white


-- int start_color(void);
{#fun unsafe start_color {} -> `Status' toStatus*#}

-- int init_pair(short pair, short f, short b);
{#fun unsafe init_pair  {shortFromInt `Int', fromColor `Color', fromColor `Color'} -> `Status' toStatus*#}

-- int init_color(short color, short r, short g, short b);
{#fun unsafe init_color {fromColor `Color', shortFromInt `Int', shortFromInt `Int', shortFromInt `Int'} -> `Status' toStatus*#}

-- bool has_colors(void);
{#fun pure has_colors {} -> `Bool'#}

-- bool can_change_color(void);
{#fun unsafe can_change_color {} -> `Bool'#}

-- int color_content(short color, short *r, short *g, short *b);
-- int pair_content(short pair, short *f, short *b);


shortFromInt :: Int -> CShort
shortFromInt = fromIntegral


#c
int color_pair(short n);
#endc
{#fun pure color_pair {shortFromInt `Int'} -> `Attr' Attr#}

------------------------------------------------------------------------
-- default_colors(3NCURSES)
------------------------------------------------------------------------

-- int use_default_colors(void);
{#fun unsafe use_default_colors {} -> `Status' toStatus*#}

-- int assume_default_colors(int fg, int bg);
{#fun unsafe assume_default_colors {fromColor' `Color', fromColor' `Color'} -> `Status' toStatus*#}

fromColor' :: Color -> CInt
fromColor' = fromIntegral . fromColor

------------------------------------------------------------------------
-- bkgd(3NCURSES)
------------------------------------------------------------------------

-- TODO: support setting of background character, not only attribute

{#fun unsafe bkgdset {chtypeFromAttr `Attr'} -> `()'#}
{#fun unsafe wbkgdset {id `Window', chtypeFromAttr `Attr'} -> `()'#}
{#fun unsafe bkgd {chtypeFromAttr `Attr'} -> `Status' toStatus*#}
{#fun unsafe wbkgd {id `Window', chtypeFromAttr `Attr'} -> `Status' toStatus*#}

-- TODO
-- chtype getbkgd(WINDOW *win);

chtypeFromAttr :: Attr -> Chtype_t
chtypeFromAttr = fromIntegral . fromAttr
