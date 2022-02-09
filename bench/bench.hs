{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
module Main where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (
  ReaderT(runReaderT),
  MonadReader,
  ask,
  )
import Criterion.Main
import Data.Attoparsec.ByteString (Result)
import Data.Attoparsec.Text (IResult(Done))
import Data.ByteString.Builder (toLazyByteString)
import qualified Data.ByteString.Lazy.Char8 as C
import Data.List.NonEmpty (NonEmpty((:|)))
import Subpar (
  AttributeValue(..),
  Command(..),
  Identifier(..),
  InfoFlag(..),
  GeneralResponse(..),
  QualIdentifier(..),
  Script(..),
  SmtHandle(..),
  Sort(..),
  Term(..),
  pop,
  push,
  setInfo,
  setOptionPrintSuccess,
  symbolSimpleSymbol,
  unparseCommand,
  unparseGeneralResponse,
  withSmtProcess,
  )
import qualified Subpar as Subpar (
  readScript,
  send,
  xfer
  )
import System.IO (
  hSetBinaryMode,
  hSetBuffering,
  BufferMode(..)
  )

main :: IO ()
main = defaultMain
  [ bench "Ex 3.10" $ nfIO ex310
  , bench "SMT-LIB-benchmarks/QF_LIA/check/bignum_lia1.smt2" $
      nfIO qfLiaCheckBignumLia1
  ]


qfLiaCheckBignumLia1 :: IO [Result GeneralResponse]
qfLiaCheckBignumLia1 = runZ3 $ do
  resultScript <- readScript "SMT-LIB-benchmarks/QF_LIA/check/bignum_lia1.smt2"
  case resultScript of
    Done _ (Script cmds) -> do
      send $ take 13 cmds
      result <- xfer [cmds !! 13]
      send [last cmds]
      return result
    _ -> error "Could not parse script."

--------------
-- Monad --
--------------
newtype Smt a = Smt{ unSmt :: ReaderT SmtHandle IO a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader SmtHandle)

runSmt :: FilePath -> [String] -> Smt a -> IO a
runSmt exe args action = withSmtProcess exe args $ \hndl -> do
  hSetBinaryMode (smtIn  hndl) True
  hSetBinaryMode (smtOut hndl) True
  hSetBuffering  (smtIn  hndl) LineBuffering
  hSetBuffering  (smtOut hndl) LineBuffering 
  runReaderT (unSmt action) hndl

runZ3 :: Smt a -> IO a
runZ3 = runSmt "z3" ["-smt2", "-in"]

xfer :: [Command] -> Smt [Result GeneralResponse]
xfer cmds = do
  hndl <- ask
  liftIO $ mapM (Subpar.xfer hndl) cmds

send :: [Command] -> Smt ()
send cmds = do
  hndl <- ask
  liftIO $ mapM_ (Subpar.send hndl) cmds

readScript :: FilePath -> Smt (Result Script)
readScript = liftIO . Subpar.readScript

{-
ex311 :: IO ()
ex311 = withSmtProcess "z3" ["-smt2", "-in"] $ \smtHandle -> do
  hSetBinaryMode (smtInt smtHandle) True
  hSetBinaryMode (smtOut smtHandle) True
  hSetBuffering  (smtIn  smtHandle) LineBuffering
  hSetBuffering  (smtOut smtHandle) LineBuffering
-}  

ex310 :: IO [Result GeneralResponse]
ex310 = runZ3 $ do
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
                            
  resps <- xfer
    [ setOptionPrintSuccess True
    , setSmtLibVer
    , setLogicQFLIA
    , declareConst "w" "Int"
    , declareConst "x" "Int"
    , declareConst "y" "Int"
    , declareConst "z" "Int"
    , assertGt "x" "y"
    , assertGt "y" "z"
    ]
  send
    [ setOptionPrintSuccess False
    , push 1
    , assertGt "z" "x"
    ]
  checkSatInfo <- xfer [CheckSat, getInfoAllStatistics]
  send
    [ pop 1
    , push 1
    ]
  checkSatResp <- xfer [CheckSat]
  send [Exit]
  return $ resps ++ checkSatInfo ++ checkSatResp
  
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
