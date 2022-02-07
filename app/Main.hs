{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE LambdaCase                 #-}
module Main where

import Control.Monad.Reader
import Data.Attoparsec.Text (IResult(Done))
import Data.Attoparsec.ByteString (Result)
import Data.ByteString.Builder (toLazyByteString)
import qualified Data.ByteString.Lazy.Char8 as C
import Data.List.NonEmpty (NonEmpty((:|)))
import Subpar (
  AttributeValue(..),
  Command(..),
  GeneralResponse(..),
  Identifier(..),
  InfoFlag(..),
  QualIdentifier(..),
  Script(..),
  SmtHandle(..),
  Sort(..),
  Term(..),
  pop,
  push,
  readScript,
  setInfo,
  setOptionPrintSuccess,
  symbolSimpleSymbol,
  unparseCommand,
  unparseGeneralResponse,
  withSmtProcess,
  writeScript
  )
import qualified Subpar as Subpar (
  send,
  xfer
  )
import System.IO (
  hSetBinaryMode,
  hSetBuffering,
  BufferMode(..)
  )

-----------
-- Monad --
-----------
newtype Smt a = Smt{ unSmt :: ReaderT SmtHandle IO a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader SmtHandle)

runSmt :: FilePath -> [String] -> Smt a -> IO a
runSmt exe args action = withSmtProcess exe args $ \smtHndl -> do
  hSetBinaryMode (smtIn  smtHndl) True
  hSetBinaryMode (smtOut smtHndl) True
  hSetBuffering  (smtIn  smtHndl) LineBuffering
  hSetBuffering  (smtOut smtHndl) LineBuffering
  runReaderT (unSmt action) smtHndl

xfer :: [Command] -> Smt [Result GeneralResponse]
xfer cmds = do
  hndl <- ask
  liftIO $ mapM (Subpar.xfer hndl) cmds

send :: [Command] -> Smt ()
send cmds = do
  hndl <- ask
  liftIO $ mapM_ (Subpar.send hndl) cmds

main :: IO ()
main = do
  putStrLn "Example 3.10 demo start."
  ex310
  putStrLn "Example 3.10 demo stop."
  putStrLn "Example 3.11 demo start."
  ex311
  putStrLn "Example 3.11 demo stop."
  putStrLn "Script demo start."
  readScript "SMT-LIB-benchmarks/QF_LIA/check/bignum_lia1.smt2" >>= \case
    Done _ (Script cmds) -> 
      runSmt "z3" ["-smt2", "-in"] $ do
        send $ take 13 cmds
        result <- xfer [cmds !! 14]
        send [last cmds] -- exit
        printResults result
    _ -> error "Could not parse script."
  putStrLn "Script demo stop."


ex311 :: IO ()
ex311 = runSmt "z3" ["-smt2", "-in"] $ do
  return ()  

ex310 :: IO ()
ex310 = runSmt "z3" ["-smt2", "-in"] $ do
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
                            
  xfer
    [ setOptionPrintSuccess True
    , setSmtLibVer
    , setLogicQFLIA
    , declareConst "w" "Int"
    , declareConst "x" "Int"
    , declareConst "y" "Int"
    , declareConst "z" "Int"
    , assertGt "x" "y"
    , assertGt "y" "z"
    ] >>= printResults
  send
    [ setOptionPrintSuccess False
    , push 1
    , assertGt "z" "x"
    ]
  xfer
    [ CheckSat
    , getInfoAllStatistics
    ] >>= printResults
  send
    [ pop 1
    , push 1
    ]
  xfer [CheckSat] >>= printResults
  send [Exit]

printResults :: [Result GeneralResponse] -> Smt ()
printResults results = forM_ results $ liftIO . \case
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
