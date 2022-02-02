{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Attoparsec.Text (IResult(Done))
import Data.ByteString.Builder (toLazyByteString)
import Subpar (
  Attribute(..),
  AttributeValue(..),
  BValue(..),
  Command(..),
  Identifier(..),
  Keyword(..),
  Option(..),
  SimpleSymbol(..),
  SmtHandle(..),
  Sort(..),
  Symbol(..),
  transmit,
  transmit_,
  unparseCommand,
  withSmtProcess,
  )
import System.IO (
  hSetBinaryMode,
  hSetBuffering,
  BufferMode(..)
  )

main :: IO ()
main = withSmtProcess "z3" ["-smt2", "-in"] $ \smtHandle -> do
  hSetBinaryMode (smtIn smtHandle) True
  hSetBinaryMode (smtOut smtHandle) True
  hSetBuffering (smtIn  smtHandle) LineBuffering
  hSetBuffering (smtOut smtHandle) LineBuffering
  let printSuccess = SetOption $ OptionPrintSuccess $ BValue True
      setSmtLibVer = SetInfo $
                       AttributeKeywordAttributeValue
                         (Keyword $ SimpleSymbol "smt-lib-version")
                         (AttributeValueSymbol $ Symbol "2.6")
      setLogicQFLIA = SetLogic $ Symbol "QF_LIA"
      declareConstWInt = DeclareConst
                           (Symbol "w")
                           (Sort (IdentifierSymbol $ Symbol "Int") [])
      declareConstXInt = DeclareConst
                           (Symbol "x")
                           (Sort (IdentifierSymbol $ Symbol "Int") [])
      declareConstYInt = DeclareConst
                           (Symbol "y")
                           (Sort (IdentifierSymbol $ Symbol "Int") [])
      declareConstZInt = DeclareConst
                           (Symbol "z")
                           (Sort (IdentifierSymbol $ Symbol "Int") [])
      assertXGtY = undefined
  transmit 
    smtHandle 
    [ printSuccess
    , setSmtLibVer
    , setLogicQFLIA
    , declareConstWInt
    , declareConstXInt
    , declareConstYInt
    , declareConstZInt
    ] >>= mapM_ printResult
  transmit_ smtHandle [Exit]
  -- print $ toLazyByteString $ unparseCommand declareConstWInt
  where
    printResult = \case
      Done _ r -> print r
      r -> error $ show r
  
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
