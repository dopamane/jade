{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-|
Module      : Subpar.Extra.Monad
Description : Smt monad for tests, benches, and apps.
Copyright   : (c) David Cox 2022
License     : BSD-3-Clause
Maintainer  : dwc1295@gmail.com
-}
module Subpar.Extra.Monad (
  Smt,
  runSmt,
  runZ3,
  xferM,
  readScript,
  runScript,
) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader(ReaderT(..), MonadReader, ask)
import Data.Attoparsec.ByteString.Char8(IResult(Done), Result)
import Subpar (
  GeneralResponse(..),
  Script(..),
  SmtHandle(..),
  withSmtProcess
  )
import qualified Subpar (
  readScript,
  xferM
  )
import System.IO (
  BufferMode(LineBuffering),
  hSetBinaryMode,
  hSetBuffering
  )

-- | Smt monad.
newtype Smt a = Smt{ unSmt :: ReaderT SmtHandle IO a }
  deriving (Functor, Applicative, Monad, MonadReader SmtHandle, MonadIO)

-- | Run Smt monad.
runSmt
  :: FilePath -- ^ Executable
  -> [String] -- ^ Arguments
  -> Smt a    -- ^ Action
  -> IO a
runSmt exe args action = withSmtProcess exe args $ \hndl -> do
  hSetBinaryMode (smtIn  hndl) True
  hSetBinaryMode (smtOut hndl) True
  hSetBuffering  (smtIn  hndl) LineBuffering
  hSetBuffering  (smtOut hndl) LineBuffering
  runReaderT (unSmt action) hndl

-- | Run Smt monad using [Z3](https://github.com/Z3Prover/z3).
runZ3 :: Smt a -> IO a
runZ3 = runSmt "z3" ["-smt2", "-in"]

-- | 'mapM' of 'Subpar.xferM' over 'Script'.
xferM :: Script -> Smt [Maybe (Result GeneralResponse)]
xferM script = do
  hndl <- ask
  liftIO $ mapM (Subpar.xferM hndl) $ unScript script

-- | Lifted 'Subpar.readScript'.
readScript :: FilePath -> Smt (Result Script)
readScript = liftIO . Subpar.readScript

-- | 'readScript' then 'xferM'.
runScript :: FilePath -> Smt [Maybe (Result GeneralResponse)]
runScript file = readScript file >>= \case
  Done _ script -> xferM script
  _ -> error "Could not parse script."
