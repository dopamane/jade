module Main where

import Criterion.Main
import Subpar.Extra.Monad (runScript, runZ3)

main :: IO ()
main = defaultMain $ map benchScript benches

benches :: [FilePath]
benches =
  [ "SMT-LIB-benchmarks/QF_LIA/check/bignum_lia1.smt2"
  , "SMT-LIB-benchmarks/QF_LIA/check/bignum_lia2.smt2"
  , "SMT-LIB-benchmarks/QF_LIA/cut_lemmas/20-vars/cut_lemma_01_001.smt2"
  , "SMT-LIB-benchmarks/QF_LIA/2019-ezsmt/travellingSalesperson/SCC/tsp_rand_70_300_1155482584_0.lp.smt2"
  ]

benchScript :: FilePath -> Benchmark
benchScript file = bench file $ nfIO $ runZ3 $ runScript file
