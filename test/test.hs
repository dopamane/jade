{-# LANGUAGE LambdaCase #-}

import Control.Monad (unless, (<=<))
import Data.Attoparsec.ByteString.Char8(IResult(Done), isSpace)
import Data.ByteString.Builder (toLazyByteString)
import qualified Data.ByteString.Char8 as C (filter, readFile)
import Data.ByteString.Lazy.Char8 (toStrict, unpack)
import Data.Functor ((<&>))
import Data.Maybe (catMaybes)
import Subpar (
  readScript,
  syntaxTests,
  unparseGeneralResponse,
  unparseScript
  )
import Subpar.Extra.Monad (
  runScript,
  runZ3
  )
import Subpar.Utility (traverseDirs)
import System.Exit (exitFailure)
import System.Process (readCreateProcess, shell)
import Test.HUnit (Test(..), assertEqual, runTestTTAndExit)

main :: IO ()
main = do

  result <- syntaxTests
  unless result exitFailure

  -- runBenchmarks benchFiles
  runSyntaxBenchmarks benchFiles

runBenchmarks :: [FilePath] -> IO ()
runBenchmarks = runTestTTAndExit . TestList . map testBench <=< traverseDirs

runSyntaxBenchmarks :: [FilePath] -> IO ()
runSyntaxBenchmarks = runTestTTAndExit . TestList . map syntaxBench <=< traverseDirs

benchFiles :: [FilePath]
benchFiles =
  [ "SMT-LIB-benchmarks/QF_LIA/20180326-Bromberger/"
--  , "SMT-LIB-benchmarks/QF_LIA/2019-cmodelsdiff/boundsmodels/" ERROR
-- , "SMT-LIB-benchmarks/QF_LIA/2019-cmodelsdiff/hamiltonianCircuit/" ERROR
--  , "SMT-LIB-benchmarks/QF_LIA/2019-cmodelsdiff/mutualExclusion/" ERROR
--  , "SMT-LIB-benchmarks/QF_LIA/2019-cmodelsdiff/randomNontight/" ERROR
--  , "SMT-LIB-benchmarks/QF_LIA/2019-cmodelsdiff/stillLive/" partial LFS
--  , "SMT-LIB-benchmarks/QF_LIA/2019-cmodelsdiff/wireRouting/" partial LFS
--  , "SMT-LIB-benchmarks/QF_LIA/2019-ezsmt/incrementalScheduling/" ERROR
--  , "SMT-LIB-benchmarks/QF_LIA/20210219-Dartagnan/" partial LFS
  , "SMT-LIB-benchmarks/QF_LIA/Averest/"
  , "SMT-LIB-benchmarks/QF_LIA/CAV_2009_benchmarks/"
  , "SMT-LIB-benchmarks/QF_LIA/CIRC/"
  , "SMT-LIB-benchmarks/QF_LIA/RTCL/"
--  , "SMT-LIB-benchmarks/QF_LIA/RWS/" ERROR
  , "SMT-LIB-benchmarks/QF_LIA/arctic-matrix/"
  , "SMT-LIB-benchmarks/QF_LIA/bofill-scheduling/"
  , "SMT-LIB-benchmarks/QF_LIA/calypto/"
  , "SMT-LIB-benchmarks/QF_LIA/check/"
  , "SMT-LIB-benchmarks/QF_LIA/convert/"
  , "SMT-LIB-benchmarks/QF_LIA/cut_lemmas/"
  , "SMT-LIB-benchmarks/QF_LIA/dillig/"
  , "SMT-LIB-benchmarks/QF_LIA/fft/"
  , "SMT-LIB-benchmarks/QF_LIA/mathsat/"
  , "SMT-LIB-benchmarks/QF_LIA/miplib2003/"
  , "SMT-LIB-benchmarks/QF_LIA/nec-smt/"
  , "SMT-LIB-benchmarks/QF_LIA/pb2010/"
  , "SMT-LIB-benchmarks/QF_LIA/pidgeons/"
  , "SMT-LIB-benchmarks/QF_LIA/prime-cone/"
  , "SMT-LIB-benchmarks/QF_LIA/rings/"
  , "SMT-LIB-benchmarks/QF_LIA/rings_preprocessed/"
  , "SMT-LIB-benchmarks/QF_LIA/slacks/"
  , "SMT-LIB-benchmarks/QF_LIA/tightrhombus/"
  , "SMT-LIB-benchmarks/QF_LIA/tropical-matrix/"
  , "SMT-LIB-benchmarks/QF_LIA/wisa/"
  ]
{-
  [ -- "SMT-LIB-benchmarks/QF_LIA/20210219-Dartagnan/ConcurrencySafety-Main/45_monabsex1_vs-O0.smt2"
  , "SMT-LIB-benchmarks/QF_LIA/convert/convert-jpg2gif-query-1139.smt2"
  , "SMT-LIB-benchmarks/QF_LIA/cut_lemmas/10-vars/cut_lemma_01_001.smt2"
  , "SMT-LIB-benchmarks/QF_LIA/mathsat/FISCHER1-1-fair.smt2"
  ]
-}
testBench :: FilePath -> Test
testBench file = TestCase $ do
  expected <- lines <$> z3Shell file
  actual   <- z3Subpar file
  assertEqual "shell == subpar" expected actual

syntaxBench :: FilePath -> Test
syntaxBench file = TestCase $ do
  let message = file ++ " : original == unparseScript . parseScript"
  expected <- removeSpace <$> C.readFile file
  actual   <- removeSpace . outputScript <$> readScript file
  assertEqual message expected actual
  where
    removeSpace = C.filter (not . isSpace)
    outputScript = \case
      Done _ r -> toStrict $ toLazyByteString $ unparseScript r
      _ -> error "Could not parse script."

z3Shell :: FilePath -> IO String
z3Shell file = readCreateProcess z3Process ""
  where
    z3Process = shell $ "z3 -smt2 " ++ file

z3Subpar :: FilePath -> IO [String]
z3Subpar file = do
  output <- runZ3 $ runScript file
  return $ catMaybes output <&> \case
    Done _ response ->
      unpack $ toLazyByteString $ unparseGeneralResponse response
    _ -> error "Could not parse response."
