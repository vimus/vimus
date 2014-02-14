module UI.Curses.Type where

import Foreign.C.Types
import Foreign.Ptr (Ptr)

#include "mycurses.h"

{#pointer *WINDOW as Window newtype#}

{-
Wrapper types corresponding to typedefs on the C side,
to avoid making assumptions about what their underlying
types are (e.g., the underlying type of chtype is
user-configurable).
-}
type Attr_t   = {#type attr_t #}
type Chtype_t = {#type chtype #}
type Wint_t   = {#type wint_t #}
