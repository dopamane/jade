module Jade.Syntax (
    -- ** Tokens

    -- *** Numeral
    Numeral (..),
    parseNumeral,
    unparseNumeral,

    -- *** Decimal
    Decimal (..),
    parseDecimal,
    unparseDecimal,

    -- *** Hexadecimal
    Hexadecimal (..),
    parseHexadecimal,
    unparseHexadecimal,

    -- *** Binary
    Binary (..),
    parseBinary,
    unparseBinary,

    -- *** String
    String (..),
    parseString,
    unparseString,

    -- *** Symbol
    Symbol (..),
    parseSymbol,
    unparseSymbol,

    -- *** Keyword
    Keyword (..),
    parseKeyword,
    unparseKeyword,

    -- ** S-expressions

    -- *** SpecConstant
    SpecConstant (..),
    parseSpecConstant,
    unparseSpecConstant,

    -- *** SExpr
    SExpr (..),
    parseSExpr,
    unparseSExpr,

    -- ** Identifiers

    -- *** Index
    Index (..),
    parseIndex,
    unparseIndex,

    -- *** Identifier
    Identifier (..),
    parseIdentifier,
    unparseIdentifier,

    -- ** Sorts
    Sort (..),
    parseSort,
    unparseSort,

    -- ** Attributes

    -- *** AttributeValue
    AttributeValue (..),
    parseAttributeValue,
    unparseAttributeValue,

    -- *** Attribute
    Attribute (..),
    parseAttribute,
    unparseAttribute,

    -- ** Terms

    -- *** QualIdentifier
    QualIdentifier (..),
    parseQualIdentifier,
    unparseQualIdentifier,

    -- *** VarBinding
    VarBinding (..),
    parseVarBinding,
    unparseVarBinding,

    -- *** SortedVar
    SortedVar (..),
    parseSortedVar,
    unparseSortedVar,

    -- *** Pattern
    Pattern (..),
    parsePattern,
    unparsePattern,

    -- *** MatchCase
    MatchCase (..),
    parseMatchCase,
    unparseMatchCase,

    -- *** Term
    Term (..),
    parseTerm,
    unparseTerm,

    -- ** Theories

    -- *** SortSymbolDecl
    SortSymbolDecl (..),
    parseSortSymbolDecl,
    unparseSortSymbolDecl,

    -- *** MetaSpecConstant
    MetaSpecConstant (..),
    parseMetaSpecConstant,
    unparseMetaSpecConstant,

    -- *** FunSymbolDecl
    FunSymbolDecl (..),
    parseFunSymbolDecl,
    unparseFunSymbolDecl,

    -- *** ParFunSymbolDecl
    ParFunSymbolDecl (..),
    parseParFunSymbolDecl,
    unparseParFunSymbolDecl,

    -- *** TheoryAttribute
    TheoryAttribute (..),
    parseTheoryAttribute,
    unparseTheoryAttribute,

    -- *** TheoryDecl
    TheoryDecl (..),
    parseTheoryDecl,
    unparseTheoryDecl,

    -- ** Logics

    -- *** LogicAttribute
    LogicAttribute (..),
    parseLogicAttribute,
    unparseLogicAttribute,

    -- *** Logic
    Logic (..),
    parseLogic,
    unparseLogic,

    -- ** Info flags
    InfoFlag (..),
    parseInfoFlag,
    unparseInfoFlag,

    -- ** Command options

    -- *** BValue
    BValue (..),
    parseBValue,
    unparseBValue,

    -- *** Option
    Option (..),
    parseOption,
    unparseOption,

    -- ** Commands

    -- *** SortDec
    SortDec (..),
    parseSortDec,
    unparseSortDec,

    -- *** SelectorDec
    SelectorDec (..),
    parseSelectorDec,
    unparseSelectorDec,

    -- *** ConstructorDec
    ConstructorDec (..),
    parseConstructorDec,
    unparseConstructorDec,

    -- *** DatatypeDec
    DatatypeDec (..),
    parseDatatypeDec,
    unparseDatatypeDec,

    -- *** FunctionDec
    FunctionDec (..),
    parseFunctionDec,
    unparseFunctionDec,

    -- *** FunctionDef
    FunctionDef (..),
    parseFunctionDef,
    unparseFunctionDef,

    -- *** PropLiteral
    PropLiteral (..),
    parsePropLiteral,
    unparsePropLiteral,

    -- *** Command
    Command (..),
    parseCommand,
    unparseCommand,

    -- *** Script
    Script (..),
    parseScript,
    unparseScript,

    -- ** Command responses

    -- *** ErrorBehavior
    ErrorBehavior (..),
    parseErrorBehavior,
    unparseErrorBehavior,

    -- *** ReasonUnknown
    ReasonUnknown (..),
    parseReasonUnknown,
    unparseReasonUnknown,

    -- *** ModelResponse
    ModelResponse (..),
    parseModelResponse,
    unparseModelResponse,

    -- *** InfoResponse
    InfoResponse (..),
    parseInfoResponse,
    unparseInfoResponse,

    -- *** ValuationPair
    ValuationPair (..),
    parseValuationPair,
    unparseValuationPair,

    -- *** TValuationPair
    TValuationPair (..),
    parseTValuationPair,
    unparseTValuationPair,

    -- *** CheckSatResponse
    CheckSatResponse (..),
    parseCheckSatResponse,
    unparseCheckSatResponse,

    -- *** EchoResponse
    EchoResponse (..),
    parseEchoResponse,
    unparseEchoResponse,

    -- *** GetAssertionsResponse
    GetAssertionsResponse (..),
    parseGetAssertionsResponse,
    unparseGetAssertionsResponse,

    -- *** GetAssignmentResponse
    GetAssignmentResponse (..),
    parseGetAssignmentResponse,
    unparseGetAssignmentResponse,

    -- *** GetInfoResponse
    GetInfoResponse (..),
    parseGetInfoResponse,
    unparseGetInfoResponse,

    -- *** GetModelResponse
    GetModelResponse (..),
    parseGetModelResponse,
    unparseGetModelResponse,

    -- *** GetOptionResponse
    GetOptionResponse (..),
    parseGetOptionResponse,
    unparseGetOptionResponse,

    -- *** GetProofResponse
    GetProofResponse (..),
    parseGetProofResponse,
    unparseGetProofResponse,

    -- *** GetUnsatAssumptionsResponse
    GetUnsatAssumptionsResponse (..),
    parseGetUnsatAssumptionsResponse,
    unparseGetUnsatAssumptionsResponse,

    -- *** GetUnsatCoreResponse
    GetUnsatCoreResponse (..),
    parseGetUnsatCoreResponse,
    unparseGetUnsatCoreResponse,

    -- *** GetValueResponse
    GetValueResponse (..),
    parseGetValueResponse,
    unparseGetValueResponse,

    -- *** SpecificSuccessResponse
    SpecificSuccessResponse (..),
    parseSpecificSuccessResponse,
    unparseSpecificSuccessResponse,

    -- *** GeneralResponse
    GeneralResponse (..),
    parseGeneralResponse,
    unparseGeneralResponse,
) where

import Data.List.NonEmpty (
    NonEmpty,
 )
import Data.Text (
    Text,
 )
import Text.Megaparsec (
    Parsec,
    Tokens,
    choice,
    try,
    (<|>),
 )
import Text.Megaparsec.Char (
    string,
 )
import Prelude hiding (String)

------------
-- Tokens --
------------

-- | @\<numeral\> ::= 0 | a non-empty sequence of digits not starting with 0@
newtype Numeral = Numeral Integer
    deriving (Show, Read, Eq)

-- | Parse 'Numeral'
parseNumeral :: Parsec e Text Numeral
parseNumeral = undefined

-- | Unparse 'Numeral'
unparseNumeral :: Numeral -> Text
unparseNumeral = undefined

-- | @\<decimal\> ::= \<numeral\>.0*\<numeral\>@
newtype Decimal = Decimal Double
    deriving (Show, Read, Eq)

-- | Parse 'Decimal'
parseDecimal :: Parsec e Text Decimal
parseDecimal = undefined

-- | Unparse 'Decimal'
unparseDecimal :: Decimal -> Text
unparseDecimal = undefined

{- |
@
\<hexadecimal\> ::= #x followed by a non-empty sequence of digits and
letters from A to F, capitalized or not
@
-}
newtype Hexadecimal = Hexadecimal Integer
    deriving (Show, Read, Eq)

-- | Parse 'Hexadecimal'
parseHexadecimal :: Parsec e Text Hexadecimal
parseHexadecimal = undefined

-- | Unparse 'Hexadecimal'
unparseHexadecimal :: Hexadecimal -> Text
unparseHexadecimal = undefined

-- | @\<binary\> ::= #b followed by a non-empty sequence of 0 and 1 characters@
newtype Binary = Binary Integer
    deriving (Show, Read, Eq)

-- | Parse 'Binary'
parseBinary :: Parsec e Text Binary
parseBinary = undefined

-- | Unparse 'Binary'
unparseBinary :: Binary -> Text
unparseBinary = undefined

{- |
@
\<string\> ::= sequence of whitespace and printable characters in double
quotes with escape sequence ""
@
-}
newtype String = String Text
    deriving (Show, Read, Eq)

-- | Parse 'String'
parseString :: Parsec e Text String
parseString = undefined

-- | Unparse 'String'
unparseString :: String -> Text
unparseString = undefined

{- |
@
\<symbol\> ::= \<simple_symbol\>
           | a sequence of whitespace and printable characters that starts
             and ends with | and does not otherwise include | or \
@
-}
newtype Symbol = Symbol Text
    deriving (Show, Read, Eq)

-- | Parse 'Symbol'
parseSymbol :: Parsec e Text Symbol
parseSymbol = undefined

-- | Unparse 'Symbol'
unparseSymbol :: Symbol -> Text
unparseSymbol = undefined

-- | @\<keyword\> ::= :\<simple_symbol\>@
newtype Keyword = Keyword Text
    deriving (Show, Read, Eq)

-- | Parse 'Keyword'
parseKeyword :: Parsec e Text Keyword
parseKeyword = undefined

-- | Unparse 'Keyword'
unparseKeyword :: Keyword -> Text
unparseKeyword = undefined

-------------------
-- S-expressions --
-------------------

{- |
@
\<spec_constant\> ::= \<numeral\>
                  | \<decimal\>
                  | \<hexadecimal\>
                  | \<binary\>
                  | \<string\>
@
-}
data SpecConstant
    = -- | \<numeral\>
      SpecConstantNumeral Numeral
    | -- | \<decimal\>
      SpecConstantDecimal Decimal
    | -- | \<hexadecimal\>
      SpecConstantHexadecimal Hexadecimal
    | -- | \<binary\>
      SpecConstantBinary Binary
    | -- | \<string\>
      SpecConstantString String
    deriving (Show, Read, Eq)

-- | Parse 'SpecConstant'
parseSpecConstant :: Parsec e Text SpecConstant
parseSpecConstant =
    choice
        [ SpecConstantNumeral <$> try parseNumeral
        , SpecConstantDecimal <$> try parseDecimal
        , SpecConstantHexadecimal <$> try parseHexadecimal
        , SpecConstantBinary <$> try parseBinary
        , SpecConstantString <$> try parseString
        ]

-- | Unparse 'SpecConstant'
unparseSpecConstant :: SpecConstant -> Text
unparseSpecConstant = \case
    SpecConstantNumeral num -> unparseNumeral num
    SpecConstantDecimal dec -> unparseDecimal dec
    SpecConstantHexadecimal hex -> unparseHexadecimal hex
    SpecConstantBinary bin -> unparseBinary bin
    SpecConstantString str -> unparseString str

{- |
@
\<s_expr\> ::= \<spec_constant\>
           | \<symbol\>
           | \<reserved\>
           | \<keyword\>
           | ( \<s_expr\>* )
@
-}
data SExpr
    = -- | \<spec_constant\>
      SExprSpecConstant SpecConstant
    | -- | \<symbol\>
      SExprSymbol Symbol
    | -- | \<reserved\>
      SExprReserved Text
    | -- | \<keyword\>
      SExprKeyword Keyword
    | -- | ( \<s_expr\>* )
      SExprs [SExpr]
    deriving (Show, Read, Eq)

-- | Parse 'SExpr'
parseSExpr :: Parsec e Text SExpr
parseSExpr =
    choice
        [ SExprSpecConstant <$> try parseSpecConstant
        , SExprSymbol <$> try parseSymbol
        , SExprReserved <$> try parseReserved
        , SExprKeyword <$> try parseKeyword
        , undefined
        ]

-- | Unparse 'SExpr'
unparseSExpr :: SExpr -> Text
unparseSExpr = \case
    SExprSpecConstant specConstant -> unparseSpecConstant specConstant
    SExprSymbol symbol -> unparseSymbol symbol
    SExprReserved reserved -> unparseReserved reserved
    SExprKeyword keyword -> unparseKeyword keyword
    SExprs exprs -> unwords ["(", unwords undefined, ")"]

-----------------
-- Identifiers --
-----------------

-- | @\<index\> ::= \<numeral\> | \<symbol\>@
data Index
    = -- | \<numeral\>
      IndexNumeral Numeral
    | -- | \<symbol\>
      IndexSymbol Symbol
    deriving (Show, Read, Eq)

-- | Parse 'Index'
parseIndex :: Parsec e Text Index
parseIndex =
    choice
        [ IndexNumeral <$> try parseNumeral
        , IndexSymbol <$> try parseSymbol
        ]

-- | Unparse 'Index'
unparseIndex :: Index -> Text
unparseIndex = \case
    IndexNumeral num -> unparseNumeral num
    IndexSymbol sym -> unparseSymbol sym

-- | @\<identifier\> ::= \<symbol\> | ( _ \<symbol\> \<index\>+ )@
data Identifier
    = -- | \<symbol\>
      IdentifierSymbol Symbol
    | -- | ( _ \<symbol\> \<index\>+ )
      IdentifierUnderscore Symbol (NonEmpty Index)
    deriving (Show, Read, Eq)

-- | Parse 'Identifier'
parseIdentifier :: Parsec e Text Identifier
parseIdentifier =
    choice
        [ IdentifierSymbol <$> try parseSymbol
        , undefined
        ]

-- | Unparse 'Identifier'
unparseIdentifier :: Identifier -> Text
unparseIdentifier = \case
    IdentifierSymbol sym -> unparseSymbol sym
    IdentifierUnderscore _ _ -> undefined

-----------
-- Sorts --
-----------

-- | @\<sort\> ::= \<identifier\> | ( \<identifier\> \<sort\>+ )@
data Sort
    = -- | \<identifier\>
      SortIdentifier Identifier
    | -- | ( \<identifier\> \<sort\>+ )
      SortIdentifiers Identifier (NonEmpty Sort)
    deriving (Show, Read, Eq)

-- | Parse 'Sort'
parseSort :: Parsec e Text Sort
parseSort =
    choice
        [ SortIdentifier <$> try parseIdentifier
        , undefined
        ]

-- | Unparse 'Sort'
unparseSort :: Sort -> Text
unparseSort = \case
    SortIdentifier identifier -> unparseIdentifier identifier
    SortIdentifiers identifier _ -> undefined

----------------
-- Attributes --
----------------

-- | @\<attribute_value\> ::= \<spec_constant\> | \<symbol\> | ( \<s_expr\>* )@
data AttributeValue
    = -- | \<spec_constant\>
      AttributeValueSpecConstant SpecConstant
    | -- | \<symbol\>
      AttributeValueSymbol Symbol
    | -- | ( \<s_expr\>* )
      AttributeValueSExprs [SExpr]
    deriving (Show, Read, Eq)

-- | Parse 'AttributeValue'
parseAttributeValue :: Parsec e Text AttributeValue
parseAttributeValue =
    choice
        [ AttributeValueSpecConstant <$> try parseSpecConstant
        , AttributeValueSymbol <$> try parseSymbol
        , undefined
        ]

-- | Unparse 'AttributeValue'
unparseAttributeValue :: AttributeValue -> Text
unparseAttributeValue = \case
    AttributeValueSpecConstant specConstant -> unparseSpecConstant specConstant
    AttributeValueSymbol symbol -> unparseSymbol symbol
    AttributeValueSExprs _ -> undefined

-- | @\<attribute\> ::= \<keyword\> | \<keyword\> \<attribute_value\>@
data Attribute
    = -- | \<keyword\>
      AttributeKeyword Keyword
    | -- | \<keyword\> \<attribute_value\>
      AttrbuteKeywordAttributeValue Keyword AttributeValue
    deriving (Show, Read, Eq)

-- | Parse 'Attribute'
parseAttribute :: Parsec e Text Attribute
parseAttribute =
    choice
        [ AttributeKeyword <$> parseKeyword
        , undefined
        ]

-- | Unparse 'Attribute'
unparseAttribute :: Attribute -> Text
unparseAttribute = \case
    AttributeKeyword keyword -> unparseKeyword keyword
    AttributeKeywordAttributeValue keyword attributeValue -> undefined

-----------
-- Terms --
-----------

-- | @\<qual_identifier\> ::= \<identifier\> | ( as \<identifier\> \<sort\> )@
data QualIdentifier
    = -- | \<identifier\>
      QualIdentifier Identifier
    | -- | ( as \<identifier\> \<sort\> )
      QualIdentifierAs Identifier Sort
    deriving (Show, Read, Eq)

-- | Parse 'QualIdentifier'
parseQualIdentifier :: Parsec e Text QualIdentifier
parseQualIdentifier =
    choice
        [ QualIdentifier <$> try parseIdentifier
        , undefined
        ]

-- | Unparse 'QualIdentifier'
unparseQualIdentifier :: QualIdentifier -> Text
unparseQualIdentifier = \case
    QualIdentifier identifier -> unparseIdentifier identifier
    QualIdentifierAs identifier sort -> undefined

-- | @\<var_binding\> ::= ( \<symbol\> \<term\> )@
data VarBinding = VarBinding Symbol Term
    deriving (Show, Read, Eq)

-- | Parse 'VarBinding'
parseVarBinding :: Parsec e Text VarBinding
parseVarBinding = undefined

-- | Unparse 'VarBinding'
unparseVarBinding :: VarBinding -> Text
unparseVarBinding (VarBinding symbol term) =
    unwords ["(", unparseSymbol symbol, unparseTerm term, ")"]

-- | @\<sorted_var\> ::= ( \<symbol\> \<sort\> )@
data SortedVar = SortedVar Symbol Sort
    deriving (Show, Read, Eq)

-- | <pattern> := <symbol> | ( <symbol> <symbol>+ )
data Pattern
    = -- | <symbol>
      Pattern Symbol
    | -- | ( <symbol> <symbol>+ )
      Patterns Symbol (NonEmpty Symbol)
    deriving (Show, Read, Eq)

-- | <match_case> ::= ( <pattern> <term> )
data MatchCase = MatchCase Pattern Term
    deriving (Show, Read, Eq)

{- |
 <term> ::= <spec_constant>
          | <qual_identifier
          | ( <qual_identifier> <term>+ )
          | ( let ( <var_binding>+ ) <term> )
          | ( forall ( <sorted_var>+ ) <term> )
          | ( exists ( <sorted_var>+ ) <term> )
          | ( match <term> ( <match_case>+ ) )
          | ( ! <term> <attribute>+ )
-}
data Term
    = -- | <spec_constant>
      TermSpecConstant SpecConstant
    | -- | <qual_identifier>
      TermQualIdentifier QualIdentifier
    | -- | ( <qual_identifier> <term>+ )
      TermQualIdentifiers QualIdentifier (NonEmpty Term)
    | -- | ( let ( <var_binding>+ ) <term> )
      TermLet (NonEmpty VarBinding) Term
    | -- | ( forall ( <sorted_var>+ ) <term> )
      TermForall (NonEmpty SortedVar) Term
    | -- | ( exists ( <sorted_var>+ ) <term> )
      TermExists (NonEmpty SortedVar) Term
    | -- | ( match <term> ( <match_case>+ ) )
      TermMatch Term (NonEmpty MatchCase)
    | -- | ( ! <term> <attribute>+ )
      TermExclamation Term (NonEmpty Attribute)
    deriving (Show, Read, Eq)

--------------
-- Theories --
--------------

-- | @\<sort_symbol_decl\> ::= ( \<identifier\> \<numeral\> \<attribute\>* )@
data SortSymbolDecl = SortSymbolDecl Identifier Numeral [Attribute]
    deriving (Show, Read, Eq)

-- | @\<meta_spec_constant\> ::= NUMERAL | DECIMAL | STRING@
data MetaSpecConstant
    = -- | NUMERAL
      MetaSpecConstantNumeral
    | -- | DECIMAL
      MetaSpecConstantDecimal
    | -- | STRING
      MetaSpecConstantString
    deriving (Show, Read, Eq)

-- | Parse 'MetaSpecConstant'
parseMetaSpecConstant :: Parsec e Text MetaSpecConstant
parseMetaSpecConstant =
    choice
        [ MetaSpecConstantNumeral <$ string "NUMERAL"
        , MetaSpecConstantDecimal <$ string "DECIMAL"
        , MetaSpecConstantString <$ string "STRING"
        ]

-- | Unparse 'MetaSpecConstant'
unparseMetaSpecConstant :: MetaSpecConstant -> Text
unparseMetaSpecConstant = \case
    MetaSpecConstantNumeral -> "NUMERAL"
    MetaSpecConstantDecimal -> "DECIMAL"
    MetaSpecConstantString -> "STRING"

{- |
@
\<fun_symbol_decl\> ::= ( \<spec_constant\> \<sort\> \<attribute\>* )
                    | ( \<meta_spec_constant\> \<sort\> \<attribute\>* )
                    | ( \<identifier\> \<sort\>+ \<attribute\>* )
@
-}
data FunSymbolDecl
    = -- | ( \<spec_constant\> \<sort\> \<attribute\>* )
      FunSymbolDeclSpecConstant SpecConstant Sort [Attribute]
    | -- | ( \<meta_spec_constant\> \<sort\> \<attribute\>* )
      FunSymbolDeclMetaSpecConstant MetaSpecConstant Sort [Attribute]
    | -- | ( \<identifier\> \<sort\>+ \<attribute\>* )
      FunSymbolDeclIdentifier Identifier (NonEmpty Sort) [Attribute]
    deriving (Show, Read, Eq)

{- |
@
\<par_fun_symbol_decl\> ::= \<fun_symbol_decl\>
                        | ( par ( \<symbol\>+ ) ( \<identifier\> \<sort\>+ \<attribute\>* ) )
@
-}
data ParFunSymbolDecl
    = -- | \<fun_symbol_decl\>
      ParFunSymbolDeclFunSymbolDecl FunSymbolDecl
    | -- | ( par ( \<symbol\>+ ) ( \<identifier\> \<sort\>+ \<attribute\>* ) )
      Par (NonEmpty Symbol) Identifier (NonEmpty Sort) [Attribute]
    deriving (Show, Read, Eq)

{- |
@
\<theory_attribute\> ::= :sorts ( \<sort_symbol_decl\>+ )
                     | :funs ( \<par_fun_symbol_decl\>+ )
                     | :sorts-description \<string\>
                     | :funs-description \<string\>
                     | :definition \<string\>
                     | :values \<string\>
                     | :notes \<string\>
                     | \<attribute\>
@
-}
data TheoryAttribute
    = -- | :sorts ( \<sort_symbol_decl\>+ )
      TheoryAttributeSorts (NonEmpty SortSymbolDecl)
    | -- | :funs ( \<par_fun_symbol_decl\>+ )
      TheoryAttributeFuns (NonEmpty ParFunSymbolDecl)
    | -- | :sorts-description \<string\>
      TheoryAttributeSortsDescription String
    | -- | :funs-description \<string\>
      TheoryAttributeFunsDescription String
    | -- | :defintion \<string\>
      TheoryAttributeDefinition String
    | -- | :values \<string\>
      TheoryAttributeValues String
    | -- | :notes \<string\>
      TheoryAttributeNotes String
    | -- | \<attribute\>
      TheoryAttributeAttribute Attribute
    deriving (Show, Read, Eq)

-- | @\<theory_decl\> ::= ( theory \<symbol\> \<theory_attribute\>+ )@
data TheoryDecl = TheoryDecl Symbol (NonEmpty TheoryAttribute)
    deriving (Show, Read, Eq)

------------
-- Logics --
------------

{- |
@
\<logic_attribute\> ::= :theories ( \<symbol\>+ )
                    | :language \<string\>
                    | :extensions \<string\>
                    | :values \<string\>
                    | :notes \<string\>
                    | \<attribute\>
@
-}
data LogicAttribute
    = -- | :theories ( \<symbol\>+ )
      LogicAttributeTheories (NonEmpty Symbol)
    | -- | :language \<string\>
      LogicAttributeLanguage String
    | -- | :extensions \<string\>
      LogicAttributeExtensions String
    | -- | :values \<string\>
      LogicAttributeValues String
    | -- | :notes \<string\>
      LogicAttributeNotes String
    | -- | \<attribute\>
      LogicAttributeAttribute Attribute
    deriving (Show, Read, Eq)

-- | @\<logic\> ::= ( logic \<symbol\> \<logic_attribute\>+ )@
data Logic = Logic Symbol (NonEmpty LogicAttribute)
    deriving (Show, Read, Eq)

----------------
-- Info flags --
----------------

{- |
@
\<info_flag\> ::= :all-statistics
              | :assertion-stack-levels
              | :authors
              | :error-behavior
              | :name
              | :reason-unknown
              | :version
              | \<keyword\>
@
-}
data InfoFlag
    = -- | :all-statistics
      InfoFlagAllStatistics
    | -- | :assertion-stack-levels
      InfoFlagAssertionStackLevels
    | -- | :authors
      InfoFlagAuthors
    | -- | :error-behavior
      InfoFlagErrorBehavior
    | -- | :name
      InfoFlagName
    | -- | :reason-unknown
      InfoFlagReasonUnknown
    | -- | :version
      InfoFlagVersion
    | -- | \<keyword\>
      InfoFlagKeyword
    deriving (Show, Read, Eq)

---------------------
-- Command options --
---------------------

-- | @\<b_value\> ::= true | false@
data BValue = BValue Bool
    deriving (Show, Read, Eq)

-- | Parse 'BValue'
parseBValue :: Parsec e Text BValue
parseBValue =
    choice [BValue True <$ string "true", BValue False <$ string "false"]

{- |
@
\<option\> ::= :diagnostic-output-channel \<string\>
           | :global-declaration \<b_value\>
           | :interactive-mode \<b_value\>
           | :print-success \<b_value\>
           | :produce-assertions \<b_value\>
           | :produce-assignments \<b_value\>
           | :produce-models \<b_value\>
           | :produce-proofs \<b_value\>
           | :produce-unsat-assumptions \<b_value\>
           | :produce-unsat-cores \<b_value\>
           | :random-seed \<numeral\>
           | :regular-output-channel \<string\>
           | :reproducible-resource-limit \<numeral\>
           | :verbosity \<numeral\>
           | \<attribute\>
@
-}
data Option
    = -- | :diagnostic-output-channel \<string\>
      OptionDiagnosticOutputChannel String
    | -- | :global-declarations \<b_value\>
      OptionGlobalDeclarations BValue
    | -- | :interactive-mode \<b_value\>
      OptionInteractiveMode BValue
    | -- | :print-success \<b_value\>
      OptionPrintSuccess BValue
    | -- | :produce-assertions \<b_value\>
      OptionProduceAssertions BValue
    | -- | :produce-assignments \<b_value\>
      OptionProduceAssignments BValue
    | -- | :produce-models \<b_value\>
      OptionProduceModels BValue
    | -- | :produce-proofs \<b_value\>
      OptionProduceProofs BValue
    | -- | :produce-unsat-assumptions \<b_value\>
      OptionProduceUnsatAssumptions BValue
    | -- | :produce-unsat-cores \<b_value\>
      OptionProduceUnsatCores BValue
    | -- | :random-seed \<numeral\>
      OptionRandomSeed Numeral
    | -- | :regular-output-channel \<string\>
      OptionRegularOutputChannel String
    | -- | :reproducible-resource-limit \<numeral\>
      OptionReproducibleResourceLimit Numeral
    | -- | :verbosity \<numeral\>
      OptionVerbosity Numeral
    | -- | \<attribute\>
      OptionAttribute Attribute
    deriving (Show, Read, Eq)

--------------
-- Commands --
--------------

-- | @\<sort_dec\> ::= ( \<symbol\> \<numeral\> )@
data SortDec = SortDec Symbol Numeral
    deriving (Show, Read, Eq)

-- | @\<selector_dec\> ::= ( \<symbol\> \<sort\> )@
data SelectorDec = SelectorDec Symbol Sort
    deriving (Show, Read, Eq)

-- | @\<constructor_dec\> ::= ( \<symbol\> \<selector_dec\>* )@
data ConstructorDec = ConstructorDec Symbol [SelectorDec]
    deriving (Show, Read, Eq)

{- |
@
\<datatype_dec\> ::= ( \<constructor_dec\>+ )
                 | ( par ( \<symbol\>+ ) ( \<constructor_dec\>+ ) )
@
-}
data DatatypeDec
    = -- | ( \<constructor_dec\>+ )
      DatatypeDec (NonEmpty ConstructorDec)
    | -- | ( par ( \<symbol\>+ ) ( \<constructor_dec\>+ ) )
      DatatypeDecPar (NonEmpty Symbol) (NonEmpty ConstructorDec)
    deriving (Show, Read, Eq)

-- | @\<function_dec\> ::= ( \<symbol\> ( \<sorted_var\>* ) \<sort\> )@
data FunctionDec = FunctionDec Symbol [SortedVar] Sort
    deriving (Show, Read, Eq)

-- | @\<function_def\> ::= \<symbol\> ( \<sorted_var\>* ) \<sort\> \<term\>@
data FunctionDef = FunctionDef Symbol [SortedVar] Sort Term
    deriving (Show, Read, Eq)

-- | @\<prop_literal\> ::= \<symbol\> | ( not \<symbol\> )@
data PropLiteral
    = -- | \<symbol\>
      PropLiteralSymbol Symbol
    | -- | ( not \<symbol\> )
      PropLiteralNotSymbol Symbol
    deriving (Show, Read, Eq)

{- |
@
\<command\> ::= ( assert \<term\> )
            | ( check-sat )
            | ( check-sat-assuming ( \<prop_literal\>* ) )
            | ( declare-const \<symbol\> \<sort\> )
            | ( declare-datatype \<symbol\> \<datatype_dec\> )
            | ( declare-datatypes ( \<sort_dec\>n+1 ) ( \<datatype_dec\>n+1 ) )
            | ( declare-fun \<symbol\> ( \<sort\>* ) \<sort\> )
            | ( declare-sort \<symbol\> \<numeral\> )
            | ( define-fun \<function-def\> )
            | ( define-funs-rec ( \<function_dec\>n+1 ) ( \<term\>n+1 ) )
            | ( define-sort \<symbol\> ( \<symbol\>* ) \<sort\> )
            | ( echo \<string\> )
            | ( exit )
            | ( get-assertions )
            | ( get-assignment )
            | ( get-info \<info_flag\> )
            | ( get-model )
            | ( get-option \<keyword\> )
            | ( get-proof )
            | ( get-unsat-assumptions )
            | ( get-unsat-core )
            | ( get-value ( \<term\>+ ) )
            | ( pop \<numeral\> )
            | ( push \<numeral\> )
            | ( reset )
            | ( reset-assertions )
            | ( set-info \<attribute\> )
            | ( set-logic \<symbol\> )
            | ( set-option \<option\> )
@
-}
data Command
    = -- | ( assert \<term\> )
      Assert Term
    | -- | ( check-sat )
      CheckSat
    | -- | ( check-sat-assuming ( \<prop_literal\>* ) )
      CheckSatAssuming [PropLiteral]
    | -- | ( declare-const \<symbol\> \<sort\> )
      DeclareConst Symbol Sort
    | -- | ( declare-datatype \<symbol\> \<datatype_dec\> )
      DeclareDatatype Symbol DatatypeDec
    | -- | ( declare-datatypes ( \<sort_dec\>n+1 ) ( \<datatype_dec\>n+1 ) )
      DeclareDatatypes (NonEmpty (SortDec, DatatypeDec))
    | -- | ( declare-fun \<symbol\> ( \<sort\>* ) \<sort\> )
      DeclareFun Symbol [Sort] Sort
    | -- | ( declare-sort \<symbol\> \<numeral\> )
      DeclareSort Symbol Numeral
    | -- | ( define-fun \<function_def\> )
      DefineFun FunctionDef
    | -- | ( define-fun-rec \<function_def\> )
      DefineFunRec FunctionDef
    | -- | ( define-funs-rec ( \<function_dec\>n+1 ) ( \<term\>n+1 ) )
      DefineFunsRec (NonEmpty (FunctionDec, Term))
    | -- | ( define-sort \<symbol\> ( \<symbol\>* ) \<sort\> )
      DefineSort Symbol [Symbol] Sort
    | -- | ( echo \<string\> )
      Echo String
    | -- | ( exit )
      Exit
    | -- | ( get-assertions )
      GetAssertions
    | -- | ( get-assignment )
      GetAssignment
    | -- | ( get-info \<info_flag\> )
      GetInfo InfoFlag
    | -- | ( get-model )
      GetModel
    | -- | ( get-option \<keyword\> )
      GetOption Keyword
    | -- | ( get-proof )
      GetProof
    | -- | ( get-unsat-assumptions )
      GetUnsatAssumptions
    | -- | ( get-unsat-core )
      GetUnsatCore
    | -- | ( get-value ( \<term\>+ ) )
      GetValue (NonEmpty Term)
    | -- | ( pop \<numeral\> )
      Pop Numeral
    | -- | ( push \<numeral\> )
      Push Numeral
    | -- | ( reset )
      Reset
    | -- | ( reset-assertions )
      ResetAssertions
    | -- | ( set-info \<attribute\> )
      SetInfo Attribute
    | -- | ( set-logic \<symbol\> )
      SetLogic Symbol
    | -- | ( set-option \<option\> )
      SetOption Option
    deriving (Show, Read, Eq)

-- | @\<script\> ::= \<command\>*@
newtype Script = Script [Command]
    deriving (Show, Read, Eq)

-----------------------
-- Command responses --
-----------------------

-- | @\<error-behavior\> ::= immediate-exit | continued-execution@
data ErrorBehavior
    = -- | immediate-exit
      ImmediateExit
    | -- | continued-execution
      ContinuedExecution
    deriving (Show, Read, Eq)

-- | Parse 'ErrorBehavior'
parseErrorBehavior :: Parsec e Text ErrorBehavior
parseErrorBehavior =
    choice
        [ ImmediateExit <$ string "immediate-exit"
        , ContinuedExecution <$ string "continued-execution"
        ]

-- | Unparse 'ErrorBehavior'
unparseErrorBehavior :: ErrorBehavior -> Text
unparseErrorBehavior = \case
    ImmediateExit -> "immediate-exit"
    ContinuedExecution -> "continued-execution"

-- | @\<reason-unknown\> ::= memout | incomplete | \<s_expr\>@
data ReasonUnknown
    = -- | memout
      Memout
    | -- | incomplete
      Incomplete
    | -- | \<s_expr\>
      ReasonUnknown SExpr
    deriving (Show, Read, Eq)

-- | Parse 'ReasonUnknown'
parseReasonUnknown :: Parsec e Text ReasonUnknown
parseReasonUnknown =
    choice
        [ MemOut <$ string "memout"
        , Incomplete <$ string "incomplete"
        , ReasonUnknown <$> try parseSExpr
        ]

-- | Unparse 'ReasonUnknown'
unparseReasonUnknown :: ReasonUnknown -> Text
unparseReasonUnknown = \case
    Memout -> "memout"
    Incomplete -> "incomplete"
    ReasonUnknown sexpr -> unparseSExpr sexpr

{- |
@
\<model_response\> ::= ( define-fun \<function_def\> )
                   | ( define-fun-rec \<function_def\> )
                   | ( define-funs-rec ( \<function_dec\>n+1 ) ( \<term\>n+1 ) )
@
-}
data ModelResponse
    = -- | ( define-fun \<function_def\> )
      ModelResponseDefineFun FunctionDef
    | -- | ( define-fun-rec \<function_def\> )
      ModelResponseDefineFunRec FunctionDef
    | -- | ( define-funs-rec ( \<function_dec\>n+1 ) ( \<term\>n+1 ) )
      ModelResponseDefineFunsRec (NonEmpty (FunctionDec, Term))
    deriving (Show, Read, Eq)

-- | Parse 'ModelResponse'
parseModelResponse :: Parsec e Text ModelResponse
parseModelResponse = undefined

-- | Unparse 'ModelResponse'
unparseModelResponse :: ModelResponse -> Text
unparseModelResponse = \case
    ModelResponseDefineFun functionDef ->
        unwords ["(", "define-fun", unparseFunctionDef functionDef, ")"]
    ModelResponseDefineFunRec functionDef ->
        unwords ["(", "define-fun-rec", unparseFunctionDef functionDef, ")"]
    ModelResponseDefineFunsRec _ ->
        unwords ["(", "define-funs-rec", "(", undefined, ")", "(", undefined, ")", ")"]

{- |
@
\<info_response\> ::= :assertion-stack-levels \<numeral\>
                  | :authors \<string\>
                  | :error-behavior \<error-behavior\>
                  | :name \<string\>
                  | :reason-unknown \<reason-unknown\>
                  | :version \<string\>
                  | \<attribute\>
@
-}
data InfoResponse
    = -- | :assertion-stack-levels \<numeral\>
      InfoResponseAssertionStackLevels Numeral
    | -- | :authors \<string\>
      InfoResponseAuthors String
    | -- | :error-behavior \<error-behavior\>
      InfoResponseErrorBehavior ErrorBehavior
    | -- | :name \<string\>
      InfoResponseName String
    | -- | :reason-unknown \<reason-unknown\>
      InfoResponseReasonUnknown ReasonUnknown
    | -- | :version \<string\>
      InfoResponseVersion String
    | -- | \<attribute\>
      InfoResponseAttribute Attribute
    deriving (Show, Read, Eq)

-- | Parse 'InfoResponse'
parseInfoResponse :: Parsec e Text InfoResponse
parseInfoResponse =
    choice
        [ InfoResponseAssertionStackLevels <$> try parseNumeral
        , InfoResponseAuthors <$> try parseString
        , InfoResponseErrorBehavior <$> try parseErrorBehavior
        , InfoResponseName <$> try parseString
        , InfoResponseReasonUnknown <$> try parseReasonUnknown
        , InfoResponseVersion <$> try parseString
        , InfoResponseAttribute <$> try parseAttribute
        ]

-- | Unparse 'InfoResponse'
unparseInfoResponse :: InfoResponse -> Text
unparseInfoResponse = \case
    InfoResponseAssertionStackLevels num ->
        unwords [":assertion-stack-levels", unparseNumeral num]
    InfoResponseAuthors str ->
        unwords [":authors", unparseString str]
    InfoResponseErrorBehavior errorBehavior ->
        unwords [":error-behavior", unparseErrorBehavior errorBehavior]
    InfoResponseName str ->
        unwords [":name", unparseString str]
    InfoResponseReasonUnknown reasonUnknown ->
        unwords [":reason-unknown", unparseReasonUnknown reasonUnknown]
    InfoResponseVersion str ->
        unwords [":version", unparseString str]
    InfoResponseAttribute attribute ->
        unparseAttribute attribute

-- | @\<valuation_pair\> ::= ( \<term\> \<term\> )@
data ValuationPair = ValuationPair Term Term
    deriving (Show, Read, Eq)

-- | Parse 'ValuationPair'
parseValuationPair :: Parsec e Text ValuationPair
parseValuationPair = undefined

-- | Unparse 'ValuationPair'
unparseValuationPair :: ValuationPair -> Text
unparseValuationPair = undefined

-- | @\<t_valuation_pair\> ::= ( \<symbol\> \<b_value\> )@
data TValuationPair = TValuationPair Symbol BValue
    deriving (Show, Read, Eq)

-- | Parse 'TValuationPair'
parseTValuationPair :: Parsec e Text TValuationPair
parseTValuationPair = undefined

-- | @\<check_sat_response\> ::= sat | unsat | unknown@
data CheckSatResponse
    = -- | sat
      Sat
    | -- | unsat
      Unsat
    | -- | unknown
      Unknown
    deriving (Show, Read, Eq)

-- | Parse 'CheckSatResponse'
parseCheckSatResponse :: Parsec e Text CheckSatResponse
parseCheckSatResponse =
    choice
        [ Sat <$ string "sat"
        , Unsat <$ string "unsat"
        , Unknown <$ string "unknown"
        ]

-- | @\<echo_response\> ::= \<string\>@
newtype EchoResponse = EchoResponse String
    deriving (Show, Read, Eq)

-- | Parse 'EchoResponse'
parseEchoResponse :: Parsec e Text EchoResponse
parseEchoResponse = EchoResponse <$> try parseString

-- | @\<get_assertions_response\> ::= ( \<term\>* )@
newtype GetAssertionsResponse = GetAssertionsResponse [Term]
    deriving (Show, Read, Eq)

-- | Parse 'GetAssertionsResponse'
parseGetAssertionsResponse :: Parsec e Text GetAssertionsResponse
parseGetAssertionsResponse = undefined

-- | @\<get_assignment_response\> ::= ( \<t_valuation_pair\>* )@
newtype GetAssignmentResponse = GetAssignmentResponse [TValuationPair]
    deriving (Show, Read, Eq)

-- | Parse 'GetAssignmentResponse'
parseGetAssignmentResponse :: Parsec e Text GetAssignmentResponse
parseGetAssignmentResponse = undefined

-- | @\<get_info_response\> ::= ( \<info_response\>+ )@
newtype GetInfoResponse = GetInfoResponse (NonEmpty InfoResponse)
    deriving (Show, Read, Eq)

-- | Parse 'GetInfoResponse'
parseGetInfoResponse :: Parsec e Text GetInfoResponse
parseGetInfoResponse = undefined

-- | @\<get_model_response\> ::= ( \<model_response\>* )@
newtype GetModelResponse = GetModelResponse [ModelResponse]
    deriving (Show, Read, Eq)

-- | Parse 'GetModelResponse'
parseGetModelResponse :: Parsec e Text GetModelResponse
parseGetModelResponse = undefined

-- | @\<get_option_response\> ::= \<attribute_value\>@
newtype GetOptionResponse = GetOptionResponse AttributeValue
    deriving (Show, Read, Eq)

-- | Parse 'GetOptionResponse'
parseGetOptionResponse :: Parsec e Text GetOptionResponse
parseGetOptionResponse = GetOptionResponse <$> try parseAttributeValue

-- | @\<get_proof_response\> ::= \<s_expr\>@
newtype GetProofResponse = GetProofResponse SExpr
    deriving (Show, Read, Eq)

-- | Parse 'GetProofResponse'
parseGetProofResponse :: Parsec e Text GetProofResponse
parseGetProofResponse = GetProofResponse <$> try parseSExpr

-- | @\<get_unsat_assumptions_response\> ::= ( \<symbol\>* )@
newtype GetUnsatAssumptionsResponse = GetUnsatAssumptionsResponse [Symbol]
    deriving (Show, Read, Eq)

-- | Parse 'GetUnsatAssumptionsResponse'
parseGetUnsatAssumptionsResponse :: Parsec e Text GetUnsatAssumptionsResponse
parseGetUnsatAssumptionsResponse = undefined

-- | @\<get_unsat_core_response\> ::= ( \<symbol\>* )@
newtype GetUnsatCoreResponse = GetUnsatCoreResponse [Symbol]
    deriving (Show, Read, Eq)

-- | Parse 'GetUnsatCoreResponse'
parseGetUnsatCoreResponse :: Parsec e Text GetUnsatCoreResponse
parseGetUnsatCoreResponse = undefined

-- | @\<get_value_response\> ::= ( \<valuation_pair\>+ )@
newtype GetValueResponse = GetValueResponse (NonEmpty ValuationPair)
    deriving (Show, Read, Eq)

-- | Parse 'GetValueResponse'
parseGetValueResponse :: Parsec e Text GetValueResponse
parseGetValueResponse = undefined

{- |
@
\<specific_success_response\> ::= \<check_sat_response\>
                              | \<echo_response\>
                              | \<get_assertions_response\>
                              | \<get_assignment_response\>
                              | \<get_info_response\>
                              | \<get_model_response\>
                              | \<get_option_response\>
                              | \<get_proof_response\>
                              | \<get_unsat_assumptions_response\>
                              | \<get_unsat_core_response\>
                              | \<get_value_response\>
@
-}
data SpecificSuccessResponse
    = -- | \<check_sat_response\>
      SpecificSuccessResponseCheckSatResponse CheckSatResponse
    | -- | \<echo_response\>
      SpecificSuccessResponseEchoResponse EchoResponse
    | -- | \<get_assertions_response\>
      SpecificSuccessResponseGetAssertionsResponse GetAssertionsResponse
    | -- | \<get_assignment_response\>
      SpecificSuccessResponseGetAssignmentResponse GetAssignmentResponse
    | -- | \<get_info_response\>
      SpecificSuccessResponseGetInfoResponse GetInfoResponse
    | -- | \<get_model_response\>
      SpecificSuccessResponseGetModelResponse GetModelResponse
    | -- | \<get_option_response\>
      SpecificSuccessResponseGetOptionResponse GetOptionResponse
    | -- | \<get_proof_response\>
      SpecificSuccessResponseGetProofResponse GetProofResponse
    | -- | \<get_unsat_assumptions_response\>
      SpecificSuccessResponseGetUnsatAssumptionsResponse GetUnsatAssumptionsResponse
    | -- | \<get_unsat_core_response\>
      SpecificSuccessResponseGetUnsatCoreResponse GetUnsatCoreResponse
    | -- | \<get_value_response\>
      SpecificSuccessResponseGetValueResponse GetValueResponse
    deriving (Show, Read, Eq)

-- | Parse 'SpecificSuccessResponse'
parseSpecificSuccessResponse :: Parsec e Text SpecificSuccessResponse
parseSpecificSuccessResponse =
    choice
        [ SpecificSuccessResponseCheckSatResponse <$> try parseCheckSatResponse
        , SpecificSuccessResponseEchoResponse <$> try parseEchoResponse
        , SpecificSuccessResponseGetAssertionsResponse <$> try parseGetAssertionsResponse
        , SpecificSuccessResponseGetAssignmentResponse <$> try parseGetAssignmentResponse
        , SpecificSuccessResponseGetInfoResponse <$> try parseGetInfoResponse
        , SpecificSuccessResponseGetModelResponse <$> try parseGetModelResponse
        , SpecificSuccessResponseGetOptionResponse <$> try parseGetOptionResponse
        , SpecificSuccessResponseGetProofResponse <$> try parseGetProofResponse
        , SpecificSuccessResponseGetUnsatAssumptionsResponse <$> try parseGetUnsatAssumptionsResponse
        , SpecificSuccessResponseGetUnsatCoreResponse <$> try parseGetUnsatCoreResponse
        , SpecificSuccessResponseGetValueResponse <$> try parseGetValueResponse
        ]

{- |
@
\<general_response\> ::= success
                     | \<specific_success_response\>
                     | unsupported
                     | ( error \<string\> )
@
-}
data GeneralResponse
    = -- | success
      GeneralResponseSuccess
    | -- | \<specific_success_response\>
      GeneralResponseSpecificSuccessResponse SpecificSuccessResponse
    | -- | unsupported
      GeneralResponseUnsupported
    | -- | ( error \<string\> )
      GeneralResponseError String
    deriving (Show, Read, Eq)

-- | Parse 'GeneralResponse'
parseGeneralResponse :: Parsec e Text GeneralResponse
parseGeneralResponse =
    choice
        [ GeneralResponseSuccess <$ string "success"
        , GeneralResponseSpecificSuccessResponse <$> try parseSpecificSuccessResponse
        , GeneralResponseUnsupported <$ string "unsupported"
        , undefined
        ]
