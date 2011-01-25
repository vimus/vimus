module Util (Status, toStatus, err, ok, cIntConv) where

import Foreign.C.Types

import qualified Constant

cIntConv :: Int -> CInt
cIntConv = fromIntegral

------------------------------------------------------------------------
-- error handling
------------------------------------------------------------------------
newtype Status = Status CInt

err, ok :: Status
err = Status Constant.err
ok  = Status Constant.ok

toStatus :: CInt -> IO Status
toStatus = return . Status
-- toStatus n = if n == Constant.err then fail "curses function returned ERR" else return $ Status n
