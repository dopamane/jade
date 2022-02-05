{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Attoparsec.Text (IResult(Done))
import Data.ByteString.Builder (toLazyByteString)
import Data.List.NonEmpty (NonEmpty((:|)))
import Subpar (
  Attribute(..),
  AttributeValue(..),
  BValue(..),
  Command(..),
  Identifier(..),
  InfoFlag(..),
  Keyword(..),
  Numeral(..),
  Option(..),
  QualIdentifier(..),
  SimpleSymbol(..),
  SmtHandle(..),
  Sort(..),
  Symbol(..),
  Term(..),
  printSuccess,
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
  hSetBinaryMode (smtIn  smtHandle) True
  hSetBinaryMode (smtOut smtHandle) True
  hSetBuffering  (smtIn  smtHandle) LineBuffering
  hSetBuffering  (smtOut smtHandle) LineBuffering
  let setSmtLibVer = SetInfo $
                       AttributeKeywordAttributeValue
                         (Keyword $ SimpleSymbol "smt-lib-version")
                         (AttributeValueSymbol $ 
                           SymbolSimpleSymbol $ SimpleSymbol "2.6"
                         )
      setLogicQFLIA = SetLogic $ SymbolSimpleSymbol $ SimpleSymbol "QF_LIA"
      declareConst sym srt = DeclareConst
                               (SymbolSimpleSymbol $ SimpleSymbol sym)
                               (Sort 
                                 (IdentifierSymbol $ 
                                   SymbolSimpleSymbol $ SimpleSymbol srt
                                 )
                                 []
                               )
      
      assertGt a b = Assert $ 
                       TermQualIdentifiers
                         (QualIdentifier $
                           IdentifierSymbol $
                             SymbolSimpleSymbol $
                               SimpleSymbol ">"
                         )
                         (
                           (TermQualIdentifier $
                              QualIdentifier $
                                IdentifierSymbol $
                                  SymbolSimpleSymbol $
                                    SimpleSymbol a
                           )
                           :|
                             [TermQualIdentifier $
                               QualIdentifier $
                                 IdentifierSymbol $
                                   SymbolSimpleSymbol $
                                     SimpleSymbol b
                             ]
                         )
      push1 = Push $ Numeral 1
      pop1 = Pop $ Numeral 1
      getInfoAllStatistics = GetInfo InfoFlagAllStatistics
                            
  transmit 
    smtHandle 
    [ printSuccess True
    , setSmtLibVer
    , setLogicQFLIA
    , declareConst "w" "Int"
    , declareConst "x" "Int"
    , declareConst "y" "Int"
    , declareConst "z" "Int"
    , assertGt "x" "y"
    , assertGt "y" "z"
    ] >>= mapM_ printResult
  transmit_ 
    smtHandle 
    [ printSuccess False
    , push1
    , assertGt "z" "x"
    ]
  transmit
    smtHandle
    [ CheckSat
    , getInfoAllStatistics
    ] >>= mapM_ printResult
  transmit_
    smtHandle
    [ pop1
    , push1
    ]
  transmit smtHandle [CheckSat] >>= mapM_ printResult
  transmit_ smtHandle [Exit]
  -- print $ toLazyByteString $ unparseCommand assertXGtY
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
