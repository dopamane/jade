{-# LANGUAGE StrictData #-}
{-|
Module      : Subpar.Process
Description : SMT process handling
Copyright   : (c) David Cox 2022
License     : BSD-3-Clause
Maintainer  : dwc1295@gmail.com
-}
module Subpar.Process (
  SmtHandle (..),
  withSmtProcess,
  createSmtProcess,
  cleanupSmtProcess
) where

import System.IO (Handle)
import System.Process (
  CreateProcess (std_err, std_in, std_out),
  ProcessHandle,
  StdStream (CreatePipe),
  cleanupProcess,
  createProcess,
  proc,
  withCreateProcess
  )

-- | Smt 'Handle'
data SmtHandle = SmtHandle
  { smtIn   :: Handle -- ^ Input
  , smtOut  :: Handle -- ^ Output
  , smtErr  :: Handle -- ^ Error
  , smtProc :: ProcessHandle
  }

-- | Smt 'CreateProcess'
smtProcess 
  :: FilePath -- ^ Executable
  -> [String] -- ^ Arguments
  -> CreateProcess
smtProcess exe opts = (proc exe opts){ std_in  = CreatePipe
                                     , std_out = CreatePipe
                                     , std_err = CreatePipe
                                     }

-- | Recommended function for creation and cleanup of smt process handle.
withSmtProcess 
  :: FilePath            -- ^ Executable
  -> [String]            -- ^ Arguments
  -> (SmtHandle -> IO a) -- ^ Smt action
  -> IO a
withSmtProcess exe opts action = withCreateProcess (smtProcess exe opts) $
  \(Just hIn) (Just hOut) (Just hErr) hProc -> 
    action SmtHandle{ smtIn   = hIn
                    , smtOut  = hOut
                    , smtErr  = hErr
                    , smtProc = hProc
                    }

-- | Create smt process handle. See 'withSmtProcess'.
createSmtProcess
  :: FilePath -- ^ Executable
  -> [String] -- ^ Arguments
  -> IO SmtHandle
createSmtProcess exe opts = do
  (Just hIn, Just hOut, Just hErr, hProc) <- createProcess $ smtProcess exe opts
  return SmtHandle{ smtIn   = hIn
                  , smtOut  = hOut
                  , smtErr  = hErr
                  , smtProc = hProc
                  }

-- | Cleanup smt process handle. See 'withSmtProcess'.
cleanupSmtProcess :: SmtHandle -> IO ()
cleanupSmtProcess hndl = cleanupProcess (Just hIn, Just hOut, Just hErr, hProc)
  where
    hIn   = smtIn   hndl
    hOut  = smtOut  hndl
    hErr  = smtErr  hndl
    hProc = smtProc hndl
