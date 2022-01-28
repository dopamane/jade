{-|
Module      : Jade.Syntax
Description : SMT-LIB version 2.6 syntax
Copyright   : (c) David Cox 2022
License     : BSD-3-Clause
Maintainer  : dwc1295@gmail.com
-}
module Jade.Syntax (
    -- ** Tokens

    -- *** Reserved
    Reserved(..),
    parseReserved,
    unparseReserved,

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

import Data.Attoparsec.Text (Parser, choice, string)
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text, unlines, unwords)
import Prelude hiding (String, unlines, unwords)

------------
-- Tokens --
------------

-- | Reserved words
newtype Reserved = Reserved Text
  deriving (Show, Read, Eq)

-- | Parse 'Reserved'
parseReserved :: Parser Reserved
parseReserved = undefined

-- | Unparse 'Reserved'
unparseReserved :: Reserved -> Text
unparseReserved (Reserved text) = text

-- | @\<numeral\> ::= 0 | a non-empty sequence of digits not starting with 0@
newtype Numeral = Numeral Integer
  deriving (Show, Read, Eq)

-- | Parse 'Numeral'
parseNumeral :: Parser Numeral
parseNumeral = undefined

-- | Unparse 'Numeral'
unparseNumeral :: Numeral -> Text
unparseNumeral = undefined

-- | @\<decimal\> ::= \<numeral\>.0*\<numeral\>@
newtype Decimal = Decimal Double
  deriving (Show, Read, Eq)

-- | Parse 'Decimal'
parseDecimal :: Parser Decimal
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
parseHexadecimal :: Parser Hexadecimal
parseHexadecimal = undefined

-- | Unparse 'Hexadecimal'
unparseHexadecimal :: Hexadecimal -> Text
unparseHexadecimal = undefined

-- | @\<binary\> ::= #b followed by a non-empty sequence of 0 and 1 characters@
newtype Binary = Binary Integer
  deriving (Show, Read, Eq)

-- | Parse 'Binary'
parseBinary :: Parser Binary
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
parseString :: Parser String
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
parseSymbol :: Parser Symbol
parseSymbol = undefined

-- | Unparse 'Symbol'
unparseSymbol :: Symbol -> Text
unparseSymbol = undefined

-- | @\<keyword\> ::= :\<simple_symbol\>@
newtype Keyword = Keyword Text
  deriving (Show, Read, Eq)

-- | Parse 'Keyword'
parseKeyword :: Parser Keyword
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
parseSpecConstant :: Parser SpecConstant
parseSpecConstant = choice
  [ SpecConstantNumeral     <$> parseNumeral
  , SpecConstantDecimal     <$> parseDecimal
  , SpecConstantHexadecimal <$> parseHexadecimal
  , SpecConstantBinary      <$> parseBinary
  , SpecConstantString      <$> parseString
  ]

-- | Unparse 'SpecConstant'
unparseSpecConstant :: SpecConstant -> Text
unparseSpecConstant = \case
  SpecConstantNumeral     num -> unparseNumeral     num
  SpecConstantDecimal     dec -> unparseDecimal     dec
  SpecConstantHexadecimal hex -> unparseHexadecimal hex
  SpecConstantBinary      bin -> unparseBinary      bin
  SpecConstantString      str -> unparseString      str

{- |
@
\<s_expr\> ::= \<spec_constant\>
           | \<symbol\>
           | \<reserved\>
           | \<keyword\>
           | ( \<s_expr\>* )
@
-}
data SExpr = SExprSpecConstant SpecConstant -- ^ \<spec_constant\>
           | SExprSymbol Symbol             -- ^ \<symbol\>
           | SExprReserved Reserved         -- ^ \<reserved\>
           | SExprKeyword Keyword           -- ^ \<keyword\>
           | SExprs [SExpr]                 -- ^ ( \<s_expr\>* )
  deriving (Show, Read, Eq)

-- | Parse 'SExpr'
parseSExpr :: Parser SExpr
parseSExpr = choice
  [ SExprSpecConstant <$> parseSpecConstant
  , SExprSymbol       <$> parseSymbol
  , SExprReserved     <$> parseReserved
  , SExprKeyword      <$> parseKeyword
  , undefined
  ]

-- | Unparse 'SExpr'
unparseSExpr :: SExpr -> Text
unparseSExpr = \case
  SExprSpecConstant specConstant -> unparseSpecConstant specConstant
  SExprSymbol symbol             -> unparseSymbol symbol
  SExprReserved reserved         -> unparseReserved reserved
  SExprKeyword keyword           -> unparseKeyword keyword
  SExprs exprs                   -> unwords ["(", unwords undefined, ")"]

-----------------
-- Identifiers --
-----------------

-- | @\<index\> ::= \<numeral\> | \<symbol\>@
data Index = IndexNumeral Numeral -- ^ \<numeral\>
           | IndexSymbol  Symbol  -- ^ \<symbol\>
  deriving (Show, Read, Eq)

-- | Parse 'Index'
parseIndex :: Parser Index
parseIndex = choice
  [ IndexNumeral <$> parseNumeral
  , IndexSymbol  <$> parseSymbol
  ]

-- | Unparse 'Index'
unparseIndex :: Index -> Text
unparseIndex = \case
  IndexNumeral num -> unparseNumeral num
  IndexSymbol  sym -> unparseSymbol  sym

-- | @\<identifier\> ::= \<symbol\> | ( _ \<symbol\> \<index\>+ )@
data Identifier
  = -- | \<symbol\>
    IdentifierSymbol Symbol
  | -- | ( _ \<symbol\> \<index\>+ )
    IdentifierUnderscore Symbol (NonEmpty Index)
  deriving (Show, Read, Eq)

-- | Parse 'Identifier'
parseIdentifier :: Parser Identifier
parseIdentifier = choice
  [ IdentifierSymbol <$> parseSymbol
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
data Sort = Sort Identifier [Sort]
  deriving (Show, Read, Eq)

-- | Parse 'Sort'
parseSort :: Parser Sort
parseSort = choice
  [ Sort <$> parseIdentifier <*> pure []
  , undefined
  ]

-- | Unparse 'Sort'
unparseSort :: Sort -> Text
unparseSort = \case
  Sort identifier []    -> unparseIdentifier identifier
  Sort identifier sorts -> 
    unwords [ "("
            , unparseIdentifier identifier
            , (unwords . map unparseSort) sorts
            , ")"
            ]

----------------
-- Attributes --
----------------

-- | @\<attribute_value\> ::= \<spec_constant\> | \<symbol\> | ( \<s_expr\>* )@
data AttributeValue
  = AttributeValueSpecConstant SpecConstant -- ^ \<spec_constant\>
  | AttributeValueSymbol Symbol             -- ^ \<symbol\>
  | AttributeValueSExprs [SExpr]            -- ^ ( \<s_expr\>* )
  deriving (Show, Read, Eq)

-- | Parse 'AttributeValue'
parseAttributeValue :: Parser AttributeValue
parseAttributeValue = choice
  [ AttributeValueSpecConstant <$> parseSpecConstant
  , AttributeValueSymbol       <$> parseSymbol
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
    AttributeKeywordAttributeValue Keyword AttributeValue
  deriving (Show, Read, Eq)

-- | Parse 'Attribute'
parseAttribute :: Parser Attribute
parseAttribute = choice
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
parseQualIdentifier :: Parser QualIdentifier
parseQualIdentifier = choice
  [ QualIdentifier <$> parseIdentifier
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
parseVarBinding :: Parser VarBinding
parseVarBinding = undefined

-- | Unparse 'VarBinding'
unparseVarBinding :: VarBinding -> Text
unparseVarBinding (VarBinding symbol term) =
    unwords ["(", unparseSymbol symbol, unparseTerm term, ")"]

-- | @\<sorted_var\> ::= ( \<symbol\> \<sort\> )@
data SortedVar = SortedVar Symbol Sort
    deriving (Show, Read, Eq)

-- | Parse 'SortedVar'
parseSortedVar :: Parser SortedVar
parseSortedVar = undefined

-- | Unparse 'SortedVar'
unparseSortedVar :: SortedVar -> Text
unparseSortedVar (SortedVar symbol sort) = 
  unwords ["(", unparseSymbol symbol, unparseSort sort, ")"]

-- | \<pattern\> ::= \<symbol\> | ( \<symbol\> \<symbol\>+ )
data Pattern
    = -- | \<symbol\>
      Pattern Symbol
    | -- | ( \<symbol\> \<symbol\>+ )
      Patterns Symbol (NonEmpty Symbol)
    deriving (Show, Read, Eq)

-- | Parse 'Pattern'
parsePattern :: Parser Pattern
parsePattern = undefined

-- | Unparse 'Pattern'
unparsePattern :: Pattern -> Text
unparsePattern = undefined

-- | \<match_case\> ::= ( \<pattern\> \<term\> )
data MatchCase = MatchCase Pattern Term
    deriving (Show, Read, Eq)

-- | Parse 'MatchCase'
parseMatchCase :: Parser MatchCase
parseMatchCase = undefined

-- | Unparse 'MatchCase'
unparseMatchCase :: MatchCase -> Text
unparseMatchCase = undefined

{- |
@
\<term\> ::= \<spec_constant\>
         | \<qual_identifier\>
         | ( \<qual_identifier\> \<term\>+ )
         | ( let ( \<var_binding\>+ ) \<term\> )
         | ( forall ( \<sorted_var\>+ ) \<term\> )
         | ( exists ( \<sorted_var\>+ ) \<term\> )
         | ( match \<term\> ( \<match_case\>+ ) )
         | ( ! \<term\> \<attribute\>+ )
@
-}
data Term
  = -- | \<spec_constant\>
    TermSpecConstant SpecConstant
  | -- | \<qual_identifier\>
    TermQualIdentifier QualIdentifier
  | -- | ( \<qual_identifier\> \<term\>+ )
    TermQualIdentifiers QualIdentifier (NonEmpty Term)
  | -- | ( let ( \<var_binding\>+ ) \<term\> )
    TermLet (NonEmpty VarBinding) Term
  | -- | ( forall ( \<sorted_var\>+ ) \<term\> )
    TermForall (NonEmpty SortedVar) Term
  | -- | ( exists ( \<sorted_var\>+ ) \<term\> )
    TermExists (NonEmpty SortedVar) Term
  | -- | ( match \<term\> ( \<match_case\>+ ) )
    TermMatch Term (NonEmpty MatchCase)
  | -- | ( ! \<term\> \<attribute\>+ )
    TermExclamation Term (NonEmpty Attribute)
  deriving (Show, Read, Eq)

-- | Parse 'Term'
parseTerm :: Parser Term
parseTerm = choice
  [ TermSpecConstant <$> parseSpecConstant
  , TermQualIdentifier <$> parseQualIdentifier
  , undefined
  , undefined
  , undefined
  , undefined
  , undefined
  , undefined
  ]

-- | Unparse 'Term'
unparseTerm :: Term -> Text
unparseTerm = \case
  TermSpecConstant specConstant -> unparseSpecConstant specConstant
  TermQualIdentifier qualIdentifier -> unparseQualIdentifier qualIdentifier
  _ -> undefined

--------------
-- Theories --
--------------

-- | @\<sort_symbol_decl\> ::= ( \<identifier\> \<numeral\> \<attribute\>* )@
data SortSymbolDecl = SortSymbolDecl Identifier Numeral [Attribute]
  deriving (Show, Read, Eq)

-- | Parse 'SortSymbolDecl'
parseSortSymbolDecl :: Parser SortSymbolDecl
parseSortSymbolDecl = undefined

-- | Unparse 'SortSymbolDecl'
unparseSortSymbolDecl :: SortSymbolDecl -> Text
unparseSortSymbolDecl = undefined

-- | @\<meta_spec_constant\> ::= NUMERAL | DECIMAL | STRING@
data MetaSpecConstant = MetaSpecConstantNumeral -- ^ NUMERAL
                      | MetaSpecConstantDecimal -- ^ DECIMAL
                      | MetaSpecConstantString  -- ^ STRING
  deriving (Show, Read, Eq)

-- | Parse 'MetaSpecConstant'
parseMetaSpecConstant :: Parser MetaSpecConstant
parseMetaSpecConstant = choice
  [ MetaSpecConstantNumeral <$ string "NUMERAL"
  , MetaSpecConstantDecimal <$ string "DECIMAL"
  , MetaSpecConstantString  <$ string "STRING"
  ]

-- | Unparse 'MetaSpecConstant'
unparseMetaSpecConstant :: MetaSpecConstant -> Text
unparseMetaSpecConstant = \case
  MetaSpecConstantNumeral -> "NUMERAL"
  MetaSpecConstantDecimal -> "DECIMAL"
  MetaSpecConstantString  -> "STRING"

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

-- | Parse 'FunSymbolDecl'
parseFunSymbolDecl :: Parser FunSymbolDecl
parseFunSymbolDecl = undefined

-- | Unparse 'FunSymbolDecl'
unparseFunSymbolDecl :: FunSymbolDecl -> Text
unparseFunSymbolDecl = undefined

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

-- | Parse 'ParFunSymbolDecl'
parseParFunSymbolDecl :: Parser ParFunSymbolDecl
parseParFunSymbolDecl = choice
  [ ParFunSymbolDeclFunSymbolDecl <$> parseFunSymbolDecl
  , undefined
  ]

-- | Unparse 'ParFunSymbolDecl'
unparseParFunSymbolDecl :: ParFunSymbolDecl -> Text
unparseParFunSymbolDecl = undefined

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

-- | Parse 'TheoryAttribute'
parseTheoryAttribute :: Parser TheoryAttribute
parseTheoryAttribute = undefined

-- | Unparse 'TheoryAttribute'
unparseTheoryAttribute :: TheoryAttribute -> Text
unparseTheoryAttribute = undefined

-- | @\<theory_decl\> ::= ( theory \<symbol\> \<theory_attribute\>+ )@
data TheoryDecl = TheoryDecl Symbol (NonEmpty TheoryAttribute)
  deriving (Show, Read, Eq)

-- | Parse 'TheoryDecl'
parseTheoryDecl :: Parser TheoryDecl
parseTheoryDecl = undefined

-- | Unparse 'TheoryDecl'
unparseTheoryDecl :: TheoryDecl -> Text
unparseTheoryDecl (TheoryDecl symbol _) = 
  unwords [ "("
          , "theory"
          , unparseSymbol symbol
          , undefined
          , ")"
          ]

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

-- | Parse 'LogicAttribute'
parseLogicAttribute :: Parser LogicAttribute
parseLogicAttribute = undefined

-- | Unparse 'LogicAttribute'
unparseLogicAttribute :: LogicAttribute -> Text
unparseLogicAttribute = undefined

-- | @\<logic\> ::= ( logic \<symbol\> \<logic_attribute\>+ )@
data Logic = Logic Symbol (NonEmpty LogicAttribute)
  deriving (Show, Read, Eq)

-- | Parse 'Logic'
parseLogic :: Parser Logic
parseLogic = undefined

-- | Unparse 'Logic'
unparseLogic :: Logic -> Text
unparseLogic = undefined

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
data InfoFlag = InfoFlagAllStatistics        -- ^ :all-statistics
              | InfoFlagAssertionStackLevels -- ^ :assertion-stack-levels
              | InfoFlagAuthors              -- ^ :authors
              | InfoFlagErrorBehavior        -- ^ :error-behavior
              | InfoFlagName                 -- ^ :name
              | InfoFlagReasonUnknown        -- ^ :reason-unknown
              | InfoFlagVersion              -- ^ :version
              | InfoFlagKeyword Keyword      -- ^ \<keyword\>
  deriving (Show, Read, Eq)

-- | Parse 'InfoFlag'
parseInfoFlag :: Parser InfoFlag
parseInfoFlag = choice
  [ InfoFlagAllStatistics        <$  string ":all-statistics"
  , InfoFlagAssertionStackLevels <$  string ":assertion-stack-levels"
  , InfoFlagAuthors              <$  string ":authors"
  , InfoFlagErrorBehavior        <$  string ":error-behavior"
  , InfoFlagName                 <$  string ":name"
  , InfoFlagReasonUnknown        <$  string ":reason-unknown"
  , InfoFlagVersion              <$  string ":version"
  , InfoFlagKeyword              <$> parseKeyword
  ]

-- | Unparse 'InfoFlag'
unparseInfoFlag :: InfoFlag -> Text
unparseInfoFlag = \case
  InfoFlagAllStatistics        -> ":all-statistics"
  InfoFlagAssertionStackLevels -> ":assertion-stack-levels"
  InfoFlagAuthors              -> ":authors"
  InfoFlagErrorBehavior        -> ":error-behavior"
  InfoFlagName                 -> ":name"
  InfoFlagReasonUnknown        -> ":reason-unknown"
  InfoFlagVersion              -> ":version"
  InfoFlagKeyword keyword      -> unparseKeyword keyword

---------------------
-- Command options --
---------------------

-- | @\<b_value\> ::= true | false@
data BValue = BValue Bool
  deriving (Show, Read, Eq)

-- | Parse 'BValue'
parseBValue :: Parser BValue
parseBValue = choice 
  [ BValue True  <$ string "true"
  , BValue False <$ string "false"
  ]

-- | Unparse 'BValue'
unparseBValue :: BValue -> Text
unparseBValue (BValue True)  = "true"
unparseBValue (BValue False) = "false"


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

-- | Parse 'Option'
parseOption :: Parser Option
parseOption = choice
  [ OptionDiagnosticOutputChannel   <$> parseString
  , OptionGlobalDeclarations        <$> parseBValue
  , OptionInteractiveMode           <$> parseBValue
  , OptionPrintSuccess              <$> parseBValue
  , OptionProduceAssertions         <$> parseBValue
  , OptionProduceAssignments        <$> parseBValue
  , OptionProduceModels             <$> parseBValue
  , OptionProduceProofs             <$> parseBValue
  , OptionProduceUnsatAssumptions   <$> parseBValue
  , OptionProduceUnsatCores         <$> parseBValue
  , OptionRandomSeed                <$> parseNumeral
  , OptionRegularOutputChannel      <$> parseString
  , OptionReproducibleResourceLimit <$> parseNumeral
  , OptionVerbosity                 <$> parseNumeral
  , OptionAttribute                 <$> parseAttribute
  ]

-- | Unparse 'Option'
unparseOption :: Option -> Text
unparseOption = \case
  OptionDiagnosticOutputChannel str ->
    unwords [":diagnostic-output-channel", unparseString str]
  OptionGlobalDeclarations b ->
    unwords [":global-declarations", unparseBValue b]
  OptionInteractiveMode b ->
    unwords [":interactive-mode", unparseBValue b]
  OptionPrintSuccess b ->
    unwords [":print-success", unparseBValue b]
  OptionProduceAssertions b ->
    unwords [":produce-assertions", unparseBValue b]
  OptionProduceAssignments b ->
    unwords [":produce-assignments", unparseBValue b]
  OptionProduceModels b ->
    unwords [":produce-models", unparseBValue b]
  OptionProduceProofs b ->
    unwords [":produce-proofs", unparseBValue b]
  OptionProduceUnsatAssumptions b ->
    unwords [":produce-unsat-assumptions", unparseBValue b]
  OptionProduceUnsatCores b ->
    unwords [":produce-unsat-cores", unparseBValue b]
  OptionRandomSeed n ->
    unwords [":random-seed", unparseNumeral n]
  OptionRegularOutputChannel str ->
    unwords [":regular-output-channel", unparseString str]
  OptionReproducibleResourceLimit n ->
    unwords [":reproducible-resource-limit", unparseNumeral n]
  OptionVerbosity n ->
    unwords [":verbosity", unparseNumeral n]
  OptionAttribute attribute -> unparseAttribute attribute

--------------
-- Commands --
--------------

-- | @\<sort_dec\> ::= ( \<symbol\> \<numeral\> )@
data SortDec = SortDec Symbol Numeral
    deriving (Show, Read, Eq)

-- | Parse 'SortDec'
parseSortDec :: Parser SortDec
parseSortDec = undefined

-- | Unparse 'SortDec'
unparseSortDec :: SortDec -> Text
unparseSortDec (SortDec symbol numeral) =
  unwords ["(", unparseSymbol symbol, unparseNumeral numeral, ")"]


-- | @\<selector_dec\> ::= ( \<symbol\> \<sort\> )@
data SelectorDec = SelectorDec Symbol Sort
    deriving (Show, Read, Eq)

-- | Parse 'SelectorDec'
parseSelectorDec :: Parser SelectorDec
parseSelectorDec = undefined

-- | Unparse 'SelectorDec'
unparseSelectorDec :: SelectorDec -> Text
unparseSelectorDec (SelectorDec symbol sort) =
  unwords ["(", unparseSymbol symbol, unparseSort sort, ")"]


-- | @\<constructor_dec\> ::= ( \<symbol\> \<selector_dec\>* )@
data ConstructorDec = ConstructorDec Symbol [SelectorDec]
    deriving (Show, Read, Eq)

-- | Parse 'ConstructorDec'
parseConstructorDec :: Parser ConstructorDec
parseConstructorDec = undefined

-- | Unparse 'ConstructorDec'
unparseConstructorDec :: ConstructorDec -> Text
unparseConstructorDec = undefined


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

-- | Parse 'DatatypeDec'
parseDatatypeDec :: Parser DatatypeDec
parseDatatypeDec = undefined

-- | Unparse 'DatatypeDec'
unparseDatatypeDec :: DatatypeDec -> Text
unparseDatatypeDec = undefined


-- | @\<function_dec\> ::= ( \<symbol\> ( \<sorted_var\>* ) \<sort\> )@
data FunctionDec = FunctionDec Symbol [SortedVar] Sort
    deriving (Show, Read, Eq)

-- | Parse 'FunctionDec'
parseFunctionDec :: Parser FunctionDec
parseFunctionDec = undefined

-- | Unparse 'FunctionDec'
unparseFunctionDec :: FunctionDec -> Text
unparseFunctionDec = undefined


-- | @\<function_def\> ::= \<symbol\> ( \<sorted_var\>* ) \<sort\> \<term\>@
data FunctionDef = FunctionDef Symbol [SortedVar] Sort Term
    deriving (Show, Read, Eq)

-- | Parse 'FunctionDef'
parseFunctionDef :: Parser FunctionDef
parseFunctionDef = undefined

unparseFunctionDef :: FunctionDef -> Text
unparseFunctionDef (FunctionDef symbol _ sort term) =
  unwords [ unparseSymbol symbol
          , "("
          , undefined
          , ")"
          , unparseSort sort
          , unparseTerm term
          ]


-- | @\<prop_literal\> ::= \<symbol\> | ( not \<symbol\> )@
data PropLiteral = PropLiteralSymbol Symbol    -- ^ \<symbol\>
                 | PropLiteralNotSymbol Symbol -- ^ ( not \<symbol\> )
  deriving (Show, Read, Eq)

-- | Parse 'PropLiteral'
parsePropLiteral :: Parser PropLiteral
parsePropLiteral = choice
  [ PropLiteralSymbol    <$> parseSymbol
  , undefined
  ]

-- | Unparse 'PropLiteral'
unparsePropLiteral :: PropLiteral -> Text
unparsePropLiteral = \case
  PropLiteralSymbol symbol -> unparseSymbol symbol
  PropLiteralNotSymbol symbol -> 
    unwords ["(", "not", unparseSymbol symbol, ")"]


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

-- | Parse 'Command'
parseCommand :: Parser Command
parseCommand = undefined

-- | Unparse 'Command'
unparseCommand :: Command -> Text
unparseCommand = undefined

-- | @\<script\> ::= \<command\>*@
newtype Script = Script [Command]
  deriving (Show, Read, Eq)

-- | Parse 'Script'
parseScript :: Parser Script
parseScript = undefined

-- | Unparse 'Script'
unparseScript :: Script -> Text
unparseScript (Script commands) = unlines $ map unparseCommand commands


-----------------------
-- Command responses --
-----------------------

-- | @\<error-behavior\> ::= immediate-exit | continued-execution@
data ErrorBehavior = ImmediateExit      -- ^ immediate-exit
                   | ContinuedExecution -- ^ continued-execution
  deriving (Show, Read, Eq)

-- | Parse 'ErrorBehavior'
parseErrorBehavior :: Parser ErrorBehavior
parseErrorBehavior = choice
  [ ImmediateExit      <$ string "immediate-exit"
  , ContinuedExecution <$ string "continued-execution"
  ]

-- | Unparse 'ErrorBehavior'
unparseErrorBehavior :: ErrorBehavior -> Text
unparseErrorBehavior = \case
  ImmediateExit      -> "immediate-exit"
  ContinuedExecution -> "continued-execution"

-- | @\<reason-unknown\> ::= memout | incomplete | \<s_expr\>@
data ReasonUnknown = Memout              -- ^ memout
                   | Incomplete          -- ^ incomplete
                   | ReasonUnknown SExpr -- ^ \<s_expr\>
  deriving (Show, Read, Eq)

-- | Parse 'ReasonUnknown'
parseReasonUnknown :: Parser ReasonUnknown
parseReasonUnknown = choice
  [ Memout        <$  string "memout"
  , Incomplete    <$  string "incomplete"
  , ReasonUnknown <$> parseSExpr
  ]

-- | Unparse 'ReasonUnknown'
unparseReasonUnknown :: ReasonUnknown -> Text
unparseReasonUnknown = \case
  Memout              -> "memout"
  Incomplete          -> "incomplete"
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
parseModelResponse :: Parser ModelResponse
parseModelResponse = undefined

-- | Unparse 'ModelResponse'
unparseModelResponse :: ModelResponse -> Text
unparseModelResponse = \case
  ModelResponseDefineFun functionDef ->
    unwords ["(", "define-fun", unparseFunctionDef functionDef, ")"]
  ModelResponseDefineFunRec functionDef ->
    unwords ["(", "define-fun-rec", unparseFunctionDef functionDef, ")"]
  ModelResponseDefineFunsRec _ ->
    unwords [ "("
            , "define-funs-rec"
            , "(", undefined, ")"
            , "(", undefined, ")"
            , ")"
            ]

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
parseInfoResponse :: Parser InfoResponse
parseInfoResponse = choice
  [ InfoResponseAssertionStackLevels <$> parseNumeral
  , InfoResponseAuthors              <$> parseString
  , InfoResponseErrorBehavior        <$> parseErrorBehavior
  , InfoResponseName                 <$> parseString
  , InfoResponseReasonUnknown        <$> parseReasonUnknown
  , InfoResponseVersion              <$> parseString
  , InfoResponseAttribute            <$> parseAttribute
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
parseValuationPair :: Parser ValuationPair
parseValuationPair = undefined

-- | Unparse 'ValuationPair'
unparseValuationPair :: ValuationPair -> Text
unparseValuationPair (ValuationPair a b) = 
  unwords ["(", unparseTerm a, unparseTerm b, ")"]


-- | @\<t_valuation_pair\> ::= ( \<symbol\> \<b_value\> )@
data TValuationPair = TValuationPair Symbol BValue
    deriving (Show, Read, Eq)

-- | Parse 'TValuationPair'
parseTValuationPair :: Parser TValuationPair
parseTValuationPair = undefined

-- | Unparse 'TValuationPair'
unparseTValuationPair :: TValuationPair -> Text
unparseTValuationPair (TValuationPair symbol bValue) = 
  unwords ["(", unparseSymbol symbol, unparseBValue bValue, ")"]


-- | @\<check_sat_response\> ::= sat | unsat | unknown@
data CheckSatResponse = Sat     -- ^ sat
                      | Unsat   -- ^ unsat
                      | Unknown -- ^ unknown
  deriving (Show, Read, Eq)

-- | Parse 'CheckSatResponse'
parseCheckSatResponse :: Parser CheckSatResponse
parseCheckSatResponse = choice
  [ Sat     <$ string "sat"
  , Unsat   <$ string "unsat"
  , Unknown <$ string "unknown"
  ]

-- | Unparse 'CheckSatResponse'
unparseCheckSatResponse :: CheckSatResponse -> Text
unparseCheckSatResponse = \case
  Sat     -> "sat"
  Unsat   -> "unsat"
  Unknown -> "unknown"


-- | @\<echo_response\> ::= \<string\>@
newtype EchoResponse = EchoResponse String
  deriving (Show, Read, Eq)

-- | Parse 'EchoResponse'
parseEchoResponse :: Parser EchoResponse
parseEchoResponse = EchoResponse <$> parseString

-- | Unparse 'EchoResponse'
unparseEchoResponse :: EchoResponse -> Text
unparseEchoResponse (EchoResponse str) = unparseString str


-- | @\<get_assertions_response\> ::= ( \<term\>* )@
newtype GetAssertionsResponse = GetAssertionsResponse [Term]
  deriving (Show, Read, Eq)

-- | Parse 'GetAssertionsResponse'
parseGetAssertionsResponse :: Parser GetAssertionsResponse
parseGetAssertionsResponse = undefined

-- | Unparse 'GetAssertionsResponse'
unparseGetAssertionsResponse :: GetAssertionsResponse -> Text
unparseGetAssertionsResponse (GetAssertionsResponse terms) =
  unwords ["(", (unwords . map unparseTerm) terms, ")"]


-- | @\<get_assignment_response\> ::= ( \<t_valuation_pair\>* )@
newtype GetAssignmentResponse = GetAssignmentResponse [TValuationPair]
  deriving (Show, Read, Eq)

-- | Parse 'GetAssignmentResponse'
parseGetAssignmentResponse :: Parser GetAssignmentResponse
parseGetAssignmentResponse = undefined

-- | Unparse 'GetAssignmentResponse'
unparseGetAssignmentResponse :: GetAssignmentResponse -> Text
unparseGetAssignmentResponse (GetAssignmentResponse tValuationPairs) =
  unwords ["(", (unwords . map unparseTValuationPair) tValuationPairs, ")"]


-- | @\<get_info_response\> ::= ( \<info_response\>+ )@
newtype GetInfoResponse = GetInfoResponse (NonEmpty InfoResponse)
  deriving (Show, Read, Eq)

-- | Parse 'GetInfoResponse'
parseGetInfoResponse :: Parser GetInfoResponse
parseGetInfoResponse = undefined

-- | Unparse 'GetInfoResponse'
unparseGetInfoResponse :: GetInfoResponse -> Text
unparseGetInfoResponse (GetInfoResponse _) = unwords ["(", undefined, ")"] 


-- | @\<get_model_response\> ::= ( \<model_response\>* )@
newtype GetModelResponse = GetModelResponse [ModelResponse]
  deriving (Show, Read, Eq)

-- | Parse 'GetModelResponse'
parseGetModelResponse :: Parser GetModelResponse
parseGetModelResponse = undefined

-- | Unparse 'GetModelResponse'
unparseGetModelResponse :: GetModelResponse -> Text
unparseGetModelResponse (GetModelResponse modelResponses) =
  unwords ["(", (unwords . map unparseModelResponse) modelResponses, ")"]


-- | @\<get_option_response\> ::= \<attribute_value\>@
newtype GetOptionResponse = GetOptionResponse AttributeValue
  deriving (Show, Read, Eq)

-- | Parse 'GetOptionResponse'
parseGetOptionResponse :: Parser GetOptionResponse
parseGetOptionResponse = GetOptionResponse <$> parseAttributeValue

-- | Unparse 'GetOptionResponse'
unparseGetOptionResponse :: GetOptionResponse -> Text
unparseGetOptionResponse (GetOptionResponse attributeValue) =
  unparseAttributeValue attributeValue


-- | @\<get_proof_response\> ::= \<s_expr\>@
newtype GetProofResponse = GetProofResponse SExpr
  deriving (Show, Read, Eq)

-- | Parse 'GetProofResponse'
parseGetProofResponse :: Parser GetProofResponse
parseGetProofResponse = GetProofResponse <$> parseSExpr

-- | Unparse 'GetProofResponse'
unparseGetProofResponse :: GetProofResponse -> Text
unparseGetProofResponse (GetProofResponse expr) = unparseSExpr expr


-- | @\<get_unsat_assumptions_response\> ::= ( \<symbol\>* )@
newtype GetUnsatAssumptionsResponse = GetUnsatAssumptionsResponse [Symbol]
  deriving (Show, Read, Eq)

-- | Parse 'GetUnsatAssumptionsResponse'
parseGetUnsatAssumptionsResponse :: Parser GetUnsatAssumptionsResponse
parseGetUnsatAssumptionsResponse = undefined

-- | Unparse 'GetUnsatAssumptionsResponse'
unparseGetUnsatAssumptionsResponse :: GetUnsatAssumptionsResponse -> Text
unparseGetUnsatAssumptionsResponse (GetUnsatAssumptionsResponse r) =
  unwords ["(", (unwords . map unparseSymbol) r, ")"]


-- | @\<get_unsat_core_response\> ::= ( \<symbol\>* )@
newtype GetUnsatCoreResponse = GetUnsatCoreResponse [Symbol]
  deriving (Show, Read, Eq)

-- | Parse 'GetUnsatCoreResponse'
parseGetUnsatCoreResponse :: Parser GetUnsatCoreResponse
parseGetUnsatCoreResponse = undefined

-- | Unparse 'GetUnsatCoreResponse'
unparseGetUnsatCoreResponse :: GetUnsatCoreResponse -> Text
unparseGetUnsatCoreResponse (GetUnsatCoreResponse symbols) =
  unwords ["(", (unwords . map unparseSymbol) symbols, ")"]


-- | @\<get_value_response\> ::= ( \<valuation_pair\>+ )@
newtype GetValueResponse = GetValueResponse (NonEmpty ValuationPair)
    deriving (Show, Read, Eq)

-- | Parse 'GetValueResponse'
parseGetValueResponse :: Parser GetValueResponse
parseGetValueResponse = undefined

-- | Unparse 'GetValueResponse'
unparseGetValueResponse :: GetValueResponse -> Text
unparseGetValueResponse = undefined


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
parseSpecificSuccessResponse :: Parser SpecificSuccessResponse
parseSpecificSuccessResponse = choice
  [ SpecificSuccessResponseCheckSatResponse <$> parseCheckSatResponse
  , SpecificSuccessResponseEchoResponse <$> parseEchoResponse
  , SpecificSuccessResponseGetAssertionsResponse <$> parseGetAssertionsResponse
  , SpecificSuccessResponseGetAssignmentResponse <$> parseGetAssignmentResponse
  , SpecificSuccessResponseGetInfoResponse <$> parseGetInfoResponse
  , SpecificSuccessResponseGetModelResponse <$> parseGetModelResponse
  , SpecificSuccessResponseGetOptionResponse <$> parseGetOptionResponse
  , SpecificSuccessResponseGetProofResponse <$> parseGetProofResponse
  , SpecificSuccessResponseGetUnsatAssumptionsResponse <$> parseGetUnsatAssumptionsResponse
  , SpecificSuccessResponseGetUnsatCoreResponse <$> parseGetUnsatCoreResponse
  , SpecificSuccessResponseGetValueResponse <$> parseGetValueResponse
  ]

-- | Unparse 'SpecificSuccessResponse'
unparseSpecificSuccessResponse :: SpecificSuccessResponse -> Text
unparseSpecificSuccessResponse = \case
  SpecificSuccessResponseCheckSatResponse checkSatResponse ->
    unparseCheckSatResponse checkSatResponse
  SpecificSuccessResponseEchoResponse echoResponse ->
    unparseEchoResponse echoResponse
  SpecificSuccessResponseGetAssertionsResponse getAssertionsResponse ->
    unparseGetAssertionsResponse getAssertionsResponse
  SpecificSuccessResponseGetAssignmentResponse getAssignmentResponse ->
    unparseGetAssignmentResponse getAssignmentResponse
  SpecificSuccessResponseGetInfoResponse getInfoResponse ->
    unparseGetInfoResponse getInfoResponse
  SpecificSuccessResponseGetModelResponse getModelResponse ->
    unparseGetModelResponse getModelResponse
  SpecificSuccessResponseGetOptionResponse getOptionResponse ->
    unparseGetOptionResponse getOptionResponse
  SpecificSuccessResponseGetProofResponse getProofResponse ->
    unparseGetProofResponse getProofResponse
  SpecificSuccessResponseGetUnsatAssumptionsResponse getUnsatAssumptionsResponse ->
    unparseGetUnsatAssumptionsResponse getUnsatAssumptionsResponse
  SpecificSuccessResponseGetUnsatCoreResponse getUnsatCoreResponse ->
    unparseGetUnsatCoreResponse getUnsatCoreResponse
  SpecificSuccessResponseGetValueResponse getValueResponse ->
    unparseGetValueResponse getValueResponse

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
parseGeneralResponse :: Parser GeneralResponse
parseGeneralResponse = choice
  [ GeneralResponseSuccess <$ string "success"
  , GeneralResponseSpecificSuccessResponse <$> parseSpecificSuccessResponse
  , GeneralResponseUnsupported <$ string "unsupported"
  , undefined
  ]

-- | Unparse 'GeneralResponse'
unparseGeneralResponse :: GeneralResponse -> Text
unparseGeneralResponse = \case
  GeneralResponseSuccess -> "success"
  GeneralResponseSpecificSuccessResponse specificSuccessResponse ->
    unparseSpecificSuccessResponse specificSuccessResponse
  GeneralResponseUnsupported -> "unsupported"
  GeneralResponseError str ->
    unwords ["(", "error", unparseString str, ")"]
