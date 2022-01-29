module Main where

import Data.Text (
    Text,
 )
import qualified Data.Text.IO as TIO
import Subpar (
    --    Attribute (..),
    --    Command (..),
    --    InfoFlag (..),
    --    Logic (..),
    --    Option (..),
    SmtHandle (..),
    --    readExpr,
    --    transmit,
    --    transmit_,
    withSmtProcess,
 )
import System.IO (
    BufferMode (LineBuffering),
    hSetBuffering,
 )
import Text.Megaparsec (
    errorBundlePretty,
 )

{-
readResult :: Text -> IO ()
readResult text = case readExpr "" text of
    Left bundle -> putStrLn $ errorBundlePretty bundle
    Right expr -> print expr
-}
main :: IO ()
main = putStrLn "Hello World" -- ex310
{-
ex311 :: IO ()
ex311 = withSmtProcess "z3" ["-smt2", "-in"] $ \smtHandle -> do
    hSetBuffering (smtIn smtHandle) LineBuffering
    hSetBuffering (smtOut smtHandle) LineBuffering
    transmit_
        smtHandle
        [ SetOption $ PrintSuccess False
        , SetOption $ ProduceModels True
        , DeclareConst "x" "Int"
        , DeclareConst "y" "Int"
        , DeclareFun "f" ["Int"] "Int"
        , Assert "(= (f x) (f y))"
        , Assert "(not (= x y))"
        ]
    transmit smtHandle [CheckSat, GetValue "(x y)"] >>= mapM_ TIO.putStrLn
    transmit_ smtHandle [DeclareConst "a" "(Array Int (List Int))"]
    transmit
        smtHandle
        [ CheckSat
        , GetValue "(a)"
        , GetValue "((select @const 2))"
        , GetValue "((first @list0) (rest @list0))"
        ]
        >>= mapM_ TIO.putStrLn

ex310 :: IO ()
ex310 = withSmtProcess "z3" ["-smt2", "-in"] $ \smtHandle -> do
    hSetBuffering (smtIn smtHandle) LineBuffering
    hSetBuffering (smtOut smtHandle) LineBuffering
    transmit
        smtHandle
        [ SetOption $ PrintSuccess True
        , SetInfo $ SmtLibVersion "2.6"
        , SetLogic Qflia
        , DeclareConst "w" "Int"
        , DeclareConst "x" "Int"
        , DeclareConst "y" "Int"
        , DeclareConst "z" "Int"
        , Assert "(> x y)"
        , Assert "(> y z)"
        ]
        >>= mapM_ readResult
    transmit_
        smtHandle
        [ SetOption $ PrintSuccess False
        , Push 1
        , Assert "(> z x)"
        ]
    transmit smtHandle [CheckSat, GetInfo AllStatistics] >>= mapM_ readResult
    transmit_ smtHandle [Pop 1, Push 1]
    transmit smtHandle [CheckSat] >>= mapM_ readResult
    transmit_ smtHandle [Exit]
-}
