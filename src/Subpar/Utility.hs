{-|
Module      : Subpar.Utility
Description : Utility functions
Copyright   : (c) David Cox 2022
License     : BSD-3-Clause
Maintainer  : dwc1295@gmail.com
-}
module Subpar.Utility (
  traverseDirs
) where

import Control.Monad (forM)
import System.Directory(listDirectory)
import System.FilePath((</>))
import System.Posix.Files(getFileStatus, isDirectory)

-- | [StackOverflow](https://stackoverflow.com/a/23822913/4051020)
traverseDirs :: [FilePath] -> IO [FilePath]
traverseDirs tops = fmap concat $ forM tops $ \top -> do
  s <- getFileStatus top
  if isDirectory s
    then go top
    else return [top]
  where
    go :: FilePath -> IO [FilePath]
    go dir = do
      ds <- listDirectory dir
      paths <- forM ds $ \d -> do
        let path = dir </> d
        s <- getFileStatus path
        if isDirectory s
          then go path
          else return [path]
      return $ concat paths
