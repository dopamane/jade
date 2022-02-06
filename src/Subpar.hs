{-# LANGUAGE ViewPatterns #-}
{-|
Module      : Subpar
Description : SMT interface
Copyright   : (c) David Cox 2022
License     : BSD-3-Clause
Maintainer  : dwc1295@gmail.com
-}
module Subpar (

  -- * Process
  module Subpar.Process,

  -- * Transfer
  xfer,
  send,
  recv,

  -- * Syntax
  module Subpar.Syntax

) where

import Data.Attoparsec.ByteString.Char8 (Result, parseWith)
import Data.ByteString.Builder (hPutBuilder, char8)
import qualified Data.ByteString.Char8 as C
import System.IO (hReady)

import Subpar.Process
import Subpar.Syntax

--------------
-- Transfer --
--------------

-- | Send a 'Command' and receive a 'GeneralResponse'.
xfer :: SmtHandle -> Command -> IO (Result GeneralResponse)
xfer hndl cmd = send hndl cmd >> recv hndl cmd

-- | Send a 'Command'.
send :: SmtHandle -> Command -> IO ()
send (smtIn -> hndl) cmd = hPutBuilder hndl $ unparseCommand cmd <> char8 '\n'

-- | Receive 'GeneralResponse'. 'Command' is needed to unambiguously parse a
-- 'SpecificSuccessResponse'.
-- See [StackOverflow thread](https://stackoverflow.com/q/33225837/4051020).
-- See 'xfer'.
recv :: SmtHandle -> Command -> IO (Result GeneralResponse)
recv (smtOut -> hndl) cmd =
  parseWith refill (parseGeneralResponse cmd) =<< C.hGetLine hndl
  where
    refill = do
      rdy <- hReady hndl
      if rdy
        then C.hGetLine hndl
        else return C.empty
