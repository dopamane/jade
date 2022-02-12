{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}

import Control.Monad (forM, unless)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader(ReaderT(..), MonadReader, ask)
import Data.Attoparsec.ByteString.Char8(IResult(Done), Result)
import qualified Data.ByteString.Builder as B (toLazyByteString)
import qualified Data.ByteString.Lazy.Char8 as C (unpack)
import Data.Functor ((<&>))
import Data.Maybe (catMaybes)
import Subpar (
  GeneralResponse(..),
  Script(..),
  SmtHandle(..),
  hasSpecificSuccessResponse,
  syntaxTests,
  unparseGeneralResponse,
  withSmtProcess
  )
import qualified Subpar (
  readScript,
  send,
  xfer,
  )
import System.Exit (exitFailure)
import System.IO (
  BufferMode(..),
  hSetBinaryMode,
  hSetBuffering,
  stdout,
  stderr
  )
import System.Process (CreateProcess, readCreateProcess, shell)
import Test.HUnit (Test(..), assertEqual, runTestTTAndExit)

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering

  result <- syntaxTests
  unless result exitFailure

  runTestTTAndExit $ TestList $ map testBench benchFiles

benchFiles :: [FilePath]
benchFiles =
  [ "SMT-LIB-benchmarks/QF_LIA/check/bignum_lia1.smt2"
  , "SMT-LIB-benchmarks/QF_LIA/check/bignum_lia2.smt2"
  , "SMT-LIB-benchmarks/QF_LIA/check/int_incompleteness1.smt2"
  , "SMT-LIB-benchmarks/QF_LIA/check/int_incompleteness2.smt2"
  , "SMT-LIB-benchmarks/QF_LIA/check/int_incompleteness3.smt2"
  ]

testBench :: FilePath -> Test
testBench file = TestCase $ do
  expected <- lines <$> z3Shell file
  actual   <- z3Subpar file
  assertEqual "shell == subpar" expected actual

z3Process :: FilePath -> CreateProcess
z3Process file = shell $ "cat " ++ file ++ " | z3 -in -smt2"

z3Shell :: FilePath -> IO String
z3Shell file = readCreateProcess (z3Process file) "" 

z3Subpar :: FilePath -> IO [String]
z3Subpar file = do
  output <- runZ3 $ runScript file
  return $ catMaybes output <&> \case
    Done _ response ->
      C.unpack $ B.toLazyByteString $ unparseGeneralResponse response
    _ -> error "Could not parse response."

-----------
-- Monad --
-----------
newtype Smt a = Smt{ unSmt :: ReaderT SmtHandle IO a }
  deriving (Functor, Applicative, Monad, MonadReader SmtHandle, MonadIO)

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

