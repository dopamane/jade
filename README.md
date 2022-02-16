# subpar :golf:

[![Haskell CI](https://github.com/dopamane/subpar/actions/workflows/haskell.yml/badge.svg?branch=main)](https://github.com/dopamane/subpar/actions/workflows/haskell.yml)

Haskell [SMT](https://smtlib.cs.uiowa.edu/) interface. Under construction. :construction:

## Build
```
git clone https://github.com/dopamane/subpar

# SMT-LIB-benchmarks submodule; skip if not running tests and benchmarks
git submodule update --init --recursive

cabal build

# documentation
cabal haddock

# test
cabal test

# benchmark
cabal bench
```

## Usage

### Obtain handle to SMT process:
```haskell
withSmtProcess
  :: FilePath            -- ^ Executable
  -> [String]            -- ^ Arguments
  -> (SmtHandle -> IO a) -- ^ Smt action
  -> IO a
```

Example:
```haskell
withSmtProcess "z3" ["-smt2", "-in"] $ \smtHandle -> do
  hSetBinaryMode (smtIn  smtHandle) True
  hSetBinaryMode (smtOut smtHandle) True
  hSetBuffering  (smtIn  smtHandle) LineBuffering
  hSetBuffering  (smtOut smtHandle) LineBuffering
  someIOAction
```

### Construct SMT-LIB v2.6 commands:
```haskell
let setSmtLibVer = setInfo
                     "smt-lib-version"
                     (Just $
                       AttributeValueSymbol $
                         symbolSimpleSymbol "2.6"
                     )
    setLogic = SetLogic $ symbolSimpleSymbol "QF_LIA"
    declareConst sym srt = DeclareConst
                             (symbolSimpleSymbol sym)
                             (Sort
                               (IdentifierSymbol $
                                 symbolSimpleSymbol srt
                               )
                               []
                             )
```

### Send commands and receive responses:
```haskell
xfer  :: SmtHandle -> Command -> IO (Result GeneralResponse)
xferM :: SmtHandle -> Command -> IO (Maybe (Result GeneralResponse))
send  :: SmtHandle -> Command -> IO ()
```

Example:
```haskell
do
  mapM (xfer smtHandle) [printSuccess, setSmtLibVer, ...] >>= mapM_ printResult
  send smtHandle Exit
```

### Read and write script files:
```haskell
readScript  :: FilePath -> IO (Result Script)
writeScript :: FilePath -> Script -> IO ()
```

## References

[SMT-LIB](https://smtlib.cs.uiowa.edu/standard.shtml)
