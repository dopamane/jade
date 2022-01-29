module Jade.Command (
    Command (..),
    Attribute (..),
    Option (..),
    Logic (..),
    InfoFlag (..),
    express,
) where

import Data.Text (Text)
import qualified Data.Text as T

data Command
    = Reset
    | Exit
    | Echo Text
    | SetInfo Attribute
    | SetOption Option
    | SetLogic Logic
    | DeclareFun Text [Text] Text
    | DeclareConst Text Text
    | Assert Text
    | Push Int
    | Pop Int
    | CheckSat
    | GetInfo InfoFlag
    | GetValue Text

express :: Command -> Text
express = \case
    Reset -> parens "reset"
    Exit -> parens "exit"
    Echo s -> parens "echo \"" <> s <> "\""
    SetInfo attr ->
        parens $ "set-info " <> expressAttribute attr
    SetOption opt ->
        parens $ "set-option " <> expressOption opt
    SetLogic logic ->
        parens $ "set-logic " <> expressLogic logic
    DeclareFun f args sigma ->
        parens $ "declare-fun " <> f <> " " <> parens (T.unwords args) <> " " <> sigma
    DeclareConst f sigma ->
        parens $ "declare-const " <> f <> " " <> sigma
    Assert a -> parens $ "assert " <> a
    Push n -> parens $ "push " <> T.pack (show n)
    Pop n -> parens $ "pop " <> T.pack (show n)
    CheckSat -> parens "check-sat"
    GetInfo f -> parens $ "get-info " <> expressInfoFlag f
    GetValue v -> parens $ "get-value " <> v

newtype Attribute
    = SmtLibVersion Text

expressAttribute :: Attribute -> Text
expressAttribute = \case
    SmtLibVersion version -> ":smt-lib-version " `T.append` version

data Option
    = DiagnosticOutputChannel Text
    | GlobalDeclarations Bool
    | InteractiveMode Bool
    | PrintSuccess Bool
    | ProduceAssertions Bool
    | ProduceAssignments Bool
    | ProduceModels Bool
    | ProduceProofs Bool
    | ProduceUnsatAssumptions Bool
    | ProduceUnsatCores Bool
    | RandomSeed Int
    | RegularOutputChannel Text
    | ReproducibleResourceLimit Int
    | Verbosity Int

expressOption :: Option -> Text
expressOption = \case
    DiagnosticOutputChannel chan -> ":diagnostic-output-channel " `T.append` chan
    GlobalDeclarations b -> ":global-declarations " `T.append` expressBool b
    InteractiveMode b -> ":interactive-mode " `T.append` expressBool b
    PrintSuccess b -> ":print-success " `T.append` expressBool b
    ProduceAssertions b -> ":produce-assertions " `T.append` expressBool b
    ProduceAssignments b -> ":produce-assignments " `T.append` expressBool b
    ProduceModels b -> ":produce-models " `T.append` expressBool b
    ProduceProofs b -> ":produce-proofs " `T.append` expressBool b
    ProduceUnsatAssumptions b -> ":produce-unsat-assumptions " `T.append` expressBool b
    ProduceUnsatCores b -> ":produce-unsat-cores " `T.append` expressBool b
    RandomSeed n -> ":random-seed " `T.append` T.pack (show n)
    RegularOutputChannel chan -> ":regular-output-channel " `T.append` chan
    ReproducibleResourceLimit n -> ":reproducible-resource-limit " `T.append` T.pack (show n)
    Verbosity n -> ":verbosity " `T.append` T.pack (show n)

expressBool :: Bool -> Text
expressBool True = "true"
expressBool False = "false"

data Logic
    = All
    | Qflia

expressLogic :: Logic -> Text
expressLogic = \case
    All -> "ALL"
    Qflia -> "QF_LIA"

data InfoFlag
    = AllStatistics
    | AssertionStackLevels
    | Authors
    | ErrorBehavior
    | Name
    | ReasonUnknown
    | Version

expressInfoFlag :: InfoFlag -> Text
expressInfoFlag = \case
    AllStatistics -> ":all-statistics"
    AssertionStackLevels -> ":assertion-stack-levels"
    Authors -> ":authors"
    ErrorBehavior -> ":error-behavior"
    Name -> ":name"
    ReasonUnknown -> ":reason-unknown"
    Version -> ":version"

parens :: Text -> Text
parens t = "(" <> t <> ")"
