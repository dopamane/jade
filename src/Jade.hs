module Jade (
    -- module Jade.Command,

    -- * Process
    module Jade.Process,
    -- module Jade.Expression,

    -- * Syntax
    module Jade.Syntax,
    -- transmit,
    -- transmit_,
) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

-- import Jade.Command
-- import Jade.Expression
import Jade.Process
import Jade.Syntax

{-
transmit :: SmtHandle -> [Command] -> IO [Text]
transmit hndl = mapM (\cmd -> send hndl cmd >> recv hndl)

transmit_ :: SmtHandle -> [Command] -> IO ()
transmit_ hndl = mapM_ (send hndl)

send :: SmtHandle -> Command -> IO ()
send hndl = TIO.hPutStrLn (smtIn hndl) . express

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
-}
