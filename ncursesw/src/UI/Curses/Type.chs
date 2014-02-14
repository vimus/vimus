module UI.Curses.Type where

import Foreign.C.Types
import Foreign.Ptr (Ptr)

#include "mycurses.h"

{#pointer *WINDOW as Window newtype#}

{-
Wrapper types that reflect typedef'd values on the C side so
as to avoid making assumptions about what their underlying types
are (e.g. the type of chtype is user-configurable).
-}
type Attr_t   = {#type attr_t #}
type Chtype_t = {#type chtype #}
type Wint_t   = {#type wint_t #}
