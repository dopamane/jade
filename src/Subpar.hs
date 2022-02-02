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

  -- * Transmit
  transmit,
  transmit_,

  -- ** Low-level interface
  send,
  recv,

  -- * Syntax
  module Subpar.Syntax,

) where

import Control.Monad (forM)
import Data.Attoparsec.ByteString (Result, parseWith)
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as C
import System.IO (hReady)

import Subpar.Process
import Subpar.Syntax

-- | Send 'Command's and receive 'GeneralResponse's.
transmit :: SmtHandle -> [Command] -> IO [Result GeneralResponse]
transmit hndl cmds = forM cmds $ \cmd -> do
  send hndl $ unparseCommand cmd
  parseWith (recv hndl) parseGeneralResponse =<< C.hGetLine (smtOut hndl)

-- | Send 'Command's without receiving 'GeneralResponse's.
transmit_ :: SmtHandle -> [Command] -> IO ()
transmit_ hndl = mapM_ (send hndl . unparseCommand)

-- | Send line of 'ByteString' to 'SmtHandle'. See 'transmit' and 'transmit_'.
send :: SmtHandle -> ByteString -> IO ()
send hndl = C.hPutStrLn (smtIn hndl)

-- | Receive line of 'ByteString' from 'SmtHandle'. 'recv' first checks
-- if there is at least one item available for input from the handle using
-- 'hReady'. If there is nothing available, 'recv' returns 'C.empty'. Otherwise
-- it uses 'C.hGetLine'.
-- See [StackOverflow thread](https://stackoverflow.com/q/33225837/4051020).
-- See 'transmit' for high-level interface.
recv :: SmtHandle -> IO ByteString
recv hndl = do
  isReady <- hReady $ smtOut hndl
  if isReady
    then C.hGetLine $ smtOut hndl
    else return C.empty
