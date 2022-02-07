{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Criterion.Main
import Data.Attoparsec.Text (IResult(Done))
import Data.ByteString.Builder (toLazyByteString)
import qualified Data.ByteString.Lazy.Char8 as C
import Data.List.NonEmpty (NonEmpty((:|)))
import Subpar (
  AttributeValue(..),
  Command(..),
  Identifier(..),
  InfoFlag(..),
  QualIdentifier(..),
  SmtHandle(..),
  Sort(..),
  Term(..),
  pop,
  push,
  send,
  setInfo,
  setOptionPrintSuccess,
  symbolSimpleSymbol,
  unparseCommand,
  unparseGeneralResponse,
  withSmtProcess,
  xfer,
  )
import System.IO (
  hSetBinaryMode,
  hSetBuffering,
  BufferMode(..)
  )

main :: IO ()
main = defaultMain [bench "Ex 3.10" $ nfIO ex310]

{-
ex311 :: IO ()
ex311 = withSmtProcess "z3" ["-smt2", "-in"] $ \smtHandle -> do
  hSetBinaryMode (smtInt smtHandle) True
  hSetBinaryMode (smtOut smtHandle) True
  hSetBuffering  (smtIn  smtHandle) LineBuffering
  hSetBuffering  (smtOut smtHandle) LineBuffering
-}  

ex310 :: IO ()
ex310 = withSmtProcess "z3" ["-smt2", "-in"] $ \smtHandle -> do
  hSetBinaryMode (smtIn  smtHandle) True
  hSetBinaryMode (smtOut smtHandle) True
  hSetBuffering  (smtIn  smtHandle) LineBuffering
  hSetBuffering  (smtOut smtHandle) LineBuffering
  let setSmtLibVer = setInfo
                       "smt-lib-version"
                       (Just $
                         AttributeValueSymbol $
                           symbolSimpleSymbol "2.6"
                       )
      setLogicQFLIA = SetLogic $ symbolSimpleSymbol "QF_LIA"
      declareConst sym srt = DeclareConst
                               (symbolSimpleSymbol sym)
                               (Sort 
                                 (IdentifierSymbol $ 
                                   symbolSimpleSymbol srt
                                 )
                                 []
                               )
      
      assertGt a b = Assert $ 
                       TermQualIdentifiers
                         (QualIdentifier $
                           IdentifierSymbol $
                             symbolSimpleSymbol ">"
                         )
                         (
                           (TermQualIdentifier $
                              QualIdentifier $
                                IdentifierSymbol $
                                  symbolSimpleSymbol a
                           )
                           :|
                             [TermQualIdentifier $
                               QualIdentifier $
                                 IdentifierSymbol $
                                   symbolSimpleSymbol b
                             ]
                         )
      getInfoAllStatistics = GetInfo InfoFlagAllStatistics
                            
  mapM (xfer smtHandle)
    [ setOptionPrintSuccess True
    , setSmtLibVer
    , setLogicQFLIA
    , declareConst "w" "Int"
    , declareConst "x" "Int"
    , declareConst "y" "Int"
    , declareConst "z" "Int"
    , assertGt "x" "y"
    , assertGt "y" "z"
    ] >>= mapM_ printResult
  mapM_ (send smtHandle)
    [ setOptionPrintSuccess False
    , push 1
    , assertGt "z" "x"
    ]
  mapM (xfer smtHandle)
    [ CheckSat
    , getInfoAllStatistics
    ] >>= mapM_ printResult
  mapM_ (send smtHandle)
    [ pop 1
    , push 1
    ]
  xfer smtHandle CheckSat >>= printResult
  send smtHandle Exit
  where
    printResult = \case
      Done _ r -> C.putStrLn $ toLazyByteString $ unparseGeneralResponse r
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

-}
