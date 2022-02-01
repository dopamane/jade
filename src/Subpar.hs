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
import Data.Attoparsec.Text (Result, parseWith)
import Data.Text (Text)
import qualified Data.Text    as T
import qualified Data.Text.IO as TIO
import System.IO (hReady)

import Subpar.Process
import Subpar.Syntax

-- | Send 'Command's and receive 'GeneralResponse's.
transmit :: SmtHandle -> [Command] -> IO [Result GeneralResponse]
transmit hndl cmds = forM cmds $ \cmd -> do
  send hndl $ unparseCommand cmd
  parseWith (recv hndl) parseGeneralResponse =<< TIO.hGetLine (smtOut hndl)

-- | Send 'Command's without receiving 'GeneralResponse's.
transmit_ :: SmtHandle -> [Command] -> IO ()
transmit_ hndl = mapM_ (send hndl . unparseCommand)

-- | Send line of 'Text' to 'SmtHandle'. See 'transmit' and 'transmit_'.
send :: SmtHandle -> Text -> IO ()
send hndl = TIO.hPutStrLn (smtIn hndl)

-- | Receive line of 'Text' from 'SmtHandle'. 'recv' first checks
-- if there is at least one item available for input from the handle using
-- 'hReady'. If there is nothing available, 'recv' returns 'T.empty'. Otherwise
-- it uses 'TIO.hGetLine'.
-- See [StackOverflow thread](https://stackoverflow.com/q/33225837/4051020).
-- See 'transmit' for high-level interface.
recv :: SmtHandle -> IO Text
recv hndl = do
  isReady <- hReady $ smtOut hndl
  if isReady
    then TIO.hGetLine $ smtOut hndl
    else return T.empty
