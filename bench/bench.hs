{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
module Main where

import Control.Monad (forM)
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
  hasSpecificSuccessResponse,
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
  [ --bench "Ex 3.10" $ nfIO ex310
    benchScript "SMT-LIB-benchmarks/QF_LIA/check/bignum_lia1.smt2"
  , benchScript "SMT-LIB-benchmarks/QF_LIA/check/bignum_lia2.smt2"
  , benchScript "SMT-LIB-benchmarks/QF_LIA/cut_lemmas/20-vars/cut_lemma_01_001.smt2"
  , benchScript
      "SMT-LIB-benchmarks/QF_LIA/2019-ezsmt/travellingSalesperson/SCC/tsp_rand_70_300_1155482584_0.lp.smt2"
  ]


benchScript :: FilePath -> Benchmark
benchScript file = bench file $ nfIO $ runZ3 $ runScript file

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

transmit :: Script -> Smt [Maybe (Result GeneralResponse)]
transmit script = do
  hndl <- ask
  liftIO $ forM (unScript script) $ \cmd ->
    if hasSpecificSuccessResponse cmd
      then Just    <$> Subpar.xfer hndl cmd
      else Nothing <$  Subpar.send hndl cmd

readScript :: FilePath -> Smt (Result Script)
readScript = liftIO . Subpar.readScript

runScript :: FilePath -> Smt [Maybe (Result GeneralResponse)]
runScript file = readScript file >>= \case
  Done _ script -> transmit script
  _ -> error "Could not parse script."

{-
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
-}  
