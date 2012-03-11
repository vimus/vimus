module Type where

import Foreign.C.Types
import Foreign.Ptr (Ptr)

#include "mycurses.h"

{#pointer *WINDOW as Window newtype#}

-- attr_t is an alias to chtype, which in turn is user-configurable.
-- we need to ask c2hs to figure out which type is actually used.
--
-- two synonyms are used to reflect the ncurses API.
type Attr_t   = {#type attr_t #}
type Chtype_t = {#type chtype #}
