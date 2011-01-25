module Type where

import Foreign.Ptr (Ptr)

#include "mycurses.h"

{#pointer *WINDOW as Window newtype#}
