# subpar :golf:
Haskell SMT Interface. Under construction. :construction:

## Usage

Obtain handle to smt process:
```haskell
withSmtProcess "z3" ["-smt2", "-in"] $ \smtHandle -> do
  hSetBuffering (smtIn  smtHandle) LineBuffering
  hSetBuffering (smtOut smtHandle) LineBuffering
  ...
```
