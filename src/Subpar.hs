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

  -- * Script
  readScript,
  writeScript,

  -- * Syntax
  module Subpar.Syntax

) where

import Data.Attoparsec.ByteString.Char8 (Result, parseWith)
import Data.ByteString.Builder (hPutBuilder, char8)
import qualified Data.ByteString.Builder as B (writeFile)
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as C
import System.IO (
  Handle,
  IOMode(ReadMode),
  hIsEOF,
  hReady,
  withBinaryFile,
  )

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
-- See 'xfer'.
recv :: SmtHandle -> Command -> IO (Result GeneralResponse)
recv (smtOut -> hndl) cmd =
  parseWith refill (parseGeneralResponse cmd) =<< C.hGetLine hndl
  where
    -- Refill parser with more data. Return 'C.empty' if there are no items
    -- left in the 'Handle'.
    -- See [StackOverflow thread](https://stackoverflow.com/q/33225837/4051020).
    refill = do
      rdy <- hReady hndl
      if rdy
        then C.hGetLine hndl
        else return C.empty

-- | Read script from file.
readScript :: FilePath -> IO (Result Script)
readScript file = withBinaryFile file ReadMode $ \hndl ->
  parseWith (refill hndl) parseScript =<< C.hGetLine hndl
  where
    refill hndl = do
      eof <- hIsEOF hndl
      if eof
        then return C.empty
        else C.hGetLine hndl

-- | Write script to file.
writeScript :: FilePath -> Script -> IO ()
writeScript file = B.writeFile file . unparseScript 

