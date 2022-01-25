module Jade.Process (
    SmtHandle (..),
    withSmtProcess,
) where

import System.IO (
    Handle,
 )
import System.Process (
    CreateProcess (std_err, std_in, std_out),
    ProcessHandle,
    StdStream (CreatePipe),
    proc,
    withCreateProcess,
 )

data SmtHandle = SmtHandle
    { smtIn :: !Handle
    , smtOut :: !Handle
    , smtErr :: !Handle
    , smtProc :: !ProcessHandle
    }

withSmtProcess ::
    -- | Executable
    FilePath ->
    -- | Arguments
    [String] ->
    -- | SMT Action
    (SmtHandle -> IO a) ->
    IO a
withSmtProcess exe opts action = withCreateProcess smtProcess $
    \(Just hIn) (Just hOut) (Just hErr) hProc ->
        action
            SmtHandle
                { smtIn = hIn
                , smtOut = hOut
                , smtErr = hErr
                , smtProc = hProc
                }
  where
    smtProcess =
        (proc exe opts)
            { std_in = CreatePipe
            , std_out = CreatePipe
            , std_err = CreatePipe
            }
