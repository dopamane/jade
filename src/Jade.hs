{-|
Module      : Jade
Description : Smt interface
Copyright   : (c) David Cox 2022
License     : BSD-3-Clause
Maintainer  : dwc1295@gmail.com
-}
module Jade (

  -- * Process
  module Jade.Process,

  -- * Transmit
  transmit,
  transmit_,

  -- ** Low-level interface
  send,
  recv,

    -- * Syntax
  module Jade.Syntax

) where

import Control.Monad (forM)
import Data.Text (Text)
import qualified Data.Text    as T
import qualified Data.Text.IO as TIO

import Jade.Process
import Jade.Syntax

-- | Send 'Command's and receive 'GeneralResponse's
transmit :: SmtHandle -> [Command] -> IO [GeneralResponse]
transmit hndl cmds = forM cmds $ \cmd -> do
  send hndl $ unparseCommand cmd
  parseGeneralResponse <$> recv hndl

-- | Send 'Command's without receiving 'GeneralResponse's.
transmit_ :: SmtHandle -> [Command] -> IO ()
transmit_ hndl = mapM_ (send hndl . unparseCommand)

-- | Send 'Text' to 'SmtHandle'. See 'transmit' and 'transmit_'.
send :: SmtHandle -> Text -> IO ()
send hndl = TIO.hPutStrLn (smtIn hndl)

-- | Receive 'Text' from 'SmtHandle'. See 'transmit'.
recv :: SmtHandle -> IO Text
recv hndl = T.unlines <$> readResponse 0
  where
    readResponse :: Int -> IO [Text]
    readResponse open = do
      -- always read at least one line
      line <- TIO.hGetLine $ smtOut hndl
      let deltaOpen = T.count "(" line - T.count ")" line
          totalOpen = open + deltaOpen
      -- if all parentheses closed, return line
      if totalOpen == 0
        then return [line]
        else (line :) <$> readResponse totalOpen
