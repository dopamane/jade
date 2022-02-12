{-# LANGUAGE LambdaCase #-}

import Control.Monad (unless)
import Data.Attoparsec.ByteString.Char8(IResult(Done))
import qualified Data.ByteString.Builder as B (toLazyByteString)
import qualified Data.ByteString.Lazy.Char8 as C (unpack)
import Data.Functor ((<&>))
import Data.Maybe (catMaybes)
import Subpar (
  syntaxTests,
  unparseGeneralResponse
  )
import Subpar.Extra.Monad (
  runScript,
  runZ3
  )
import System.Exit (exitFailure)
import System.IO (
  BufferMode(LineBuffering),
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
