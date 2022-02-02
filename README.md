# subpar :golf:
Haskell SMT Interface. Under construction. :construction:

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
  ...
```

### Construct SMT-LIB v2.6 commands:
```haskell
let printSuccess = SetOption $ OptionPrintSuccess $ BValue True
    setSmtLibVer = SetInfo $
                     AttributeKeywordAttributeValue
                       (Keyword $ SimpleSymbol "smt-lib-version")
                       (AttributeValueSymbol $ Symbol "2.6")
    setLogic = SetLogic $ Symbol "QF_LIA"
    declareConstWInt = DeclareConst
                         (Symbol "w")
                         (Sort (IdentifierSymbol $ Symbol "Int") [])
    ...
```

### Send commands and receive responses:
```haskell
transmit  :: SmtHandle -> [Command] -> IO [Result GeneralResponse]
transmit_ :: SmtHandle -> [Command] -> IO ()
```

Example:
```haskell
do
  transmit smtHandle [printSuccess, setSmtLibVer, ...] >>= mapM_ printResult
  transmit_ smtHandle [Exit]
```

## References

[SMT-LIB](https://smtlib.cs.uiowa.edu/standard.shtml)
