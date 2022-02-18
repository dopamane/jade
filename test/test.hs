{-# LANGUAGE LambdaCase               #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

import Control.Monad (unless)
import Data.Attoparsec.ByteString.Char8(IResult(Done), isSpace, parseOnly)
import Data.ByteString.Builder (toLazyByteString)
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as C (
  dropWhile,
  empty,
  filter,
  null,
  readFile,
  span
  )
import Data.ByteString.Lazy.Char8 (toStrict, unpack)
import Data.Functor ((<&>))
import Data.Maybe (catMaybes)
import           Hedgehog hiding (Test)
import qualified Hedgehog.Gen   as Gen
import qualified Hedgehog.Range as Range
import Subpar (
  Binary(Binary),
  Hexadecimal(Hexadecimal),
  Numeral(Numeral),
  parseBinary,
  parseHexadecimal,
  parseNumeral,
  readScript,
  unparseBinary,
  unparseGeneralResponse,
  unparseHexadecimal,
  unparseNumeral,
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
  runTestTTAndExit =<< syntaxBenchmarks benchFiles

---------------------------
-- Syntax property tests --
---------------------------

syntaxTests :: IO Bool
syntaxTests = checkSequential $ Group "syntax properties"
  [ ("prop_numeral_forward",     prop_numeral_forward)
  , ("prop_hexadecimal_forward", prop_hexadecimal_forward)
  , ("prop_binary_forward",      prop_binary_forward)
  ]

-- | 'parseNumeral' . 'unparseNumeral' == id
prop_numeral_forward :: Property
prop_numeral_forward = property $ do
  n <- forAll $ Gen.integral $ Range.linear 0 (maxBound :: Int)
  let num   = Numeral $ fromIntegral n
      numBs = toStrict $ toLazyByteString $ unparseNumeral num
  parseOnly parseNumeral numBs === Right num

-- | 'parseHexadecimal' . 'unparseHexadecimal' == id
prop_hexadecimal_forward :: Property
prop_hexadecimal_forward = property $ do
  n <- forAll $ Gen.integral $ Range.linear 0 (maxBound :: Int)
  let hex   = Hexadecimal $ fromIntegral n
      hexBs = toStrict $ toLazyByteString $ unparseHexadecimal hex
  parseOnly parseHexadecimal hexBs === Right hex

-- | 'parseBinary' . 'unparseBinary' == id
prop_binary_forward :: Property
prop_binary_forward = property $ do
  n <- forAll $ Gen.integral $ Range.linear 0 (maxBound :: Int)
  let bin   = Binary $ fromIntegral n
      binBs = toStrict $ toLazyByteString $ unparseBinary bin
  parseOnly parseBinary binBs === Right bin


----------------------------
-- Syntax benchmark tests --
----------------------------

shellBenchmarks :: [FilePath] -> IO Test
shellBenchmarks = fmap (TestList . map shellBench) . traverseDirs

syntaxBenchmarks :: [FilePath] -> IO Test
syntaxBenchmarks = fmap (TestList . map syntaxBench) . traverseDirs

benchFiles :: [FilePath]
benchFiles =
  [ "SMT-LIB-benchmarks/QF_LIA/20180326-Bromberger/"
  , "SMT-LIB-benchmarks/QF_LIA/2019-cmodelsdiff/boundsmodels/"
  , "SMT-LIB-benchmarks/QF_LIA/2019-cmodelsdiff/hamiltonianCircuit/"
--  , "SMT-LIB-benchmarks/QF_LIA/2019-cmodelsdiff/labyrinth/" LFS
  , "SMT-LIB-benchmarks/QF_LIA/2019-cmodelsdiff/mutualExclusion/"
  , "SMT-LIB-benchmarks/QF_LIA/2019-cmodelsdiff/randomNontight/"
--  , "SMT-LIB-benchmarks/QF_LIA/2019-cmodelsdiff/stillLive/" partial LFS
--  , "SMT-LIB-benchmarks/QF_LIA/2019-cmodelsdiff/wireRouting/" partial LFS
--  , "SMT-LIB-benchmarks/QF_LIA/2019-ezsmt/Labyrinth/" LFS
  , "SMT-LIB-benchmarks/QF_LIA/2019-ezsmt/incrementalScheduling/"
  , "SMT-LIB-benchmarks/QF_LIA/2019-ezsmt/routingMax/"
  , "SMT-LIB-benchmarks/QF_LIA/2019-ezsmt/routingMin/"
  , "SMT-LIB-benchmarks/QF_LIA/2019-ezsmt/travellingSalesperson/"
  , "SMT-LIB-benchmarks/QF_LIA/2019-ezsmt/weightedSequence/"
--  , "SMT-LIB-benchmarks/QF_LIA/20210219-Dartagnan/" partial LFS
  , "SMT-LIB-benchmarks/QF_LIA/Averest/"
  , "SMT-LIB-benchmarks/QF_LIA/CAV_2009_benchmarks/"
  , "SMT-LIB-benchmarks/QF_LIA/CIRC/"
  , "SMT-LIB-benchmarks/QF_LIA/RTCL/"
  , "SMT-LIB-benchmarks/QF_LIA/RWS/"
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

shellBench :: FilePath -> Test
shellBench file = TestCase $ do
  let message = file ++ " : shell == subpar"
  expected <- lines <$> z3Shell file
  actual   <- z3Subpar file
  assertEqual message expected actual

syntaxBench :: FilePath -> Test
syntaxBench file = TestCase $ do
  let message = file ++ " : original == unparseScript . parseScript"
  expected <- removeSpace . removeComments <$> C.readFile file
  actual   <- removeSpace . outputScript <$> readScript file
  assertEqual message expected actual
  where
    removeSpace = C.filter (not . isSpace)
    outputScript = \case
      Done _ r -> toStrict $ toLazyByteString $ unparseScript r
      _ -> error "Could not parse script."

removeComments :: ByteString -> ByteString
removeComments bs
  | C.null bs = C.empty
  | otherwise = t <> removeComments d'
  where
    (t, d) = C.span (/= ';') bs
    d' = C.dropWhile isSpace $ C.dropWhile (not . isNewline) d
      where
        isNewline ch = ch == '\n' || ch == '\r'

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
