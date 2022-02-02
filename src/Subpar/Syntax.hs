{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData        #-}
{-# LANGUAGE TemplateHaskell   #-}
{-|
Module      : Subpar.Syntax
Description : SMT-LIB version 2.6 syntax
Copyright   : (c) David Cox 2022
License     : BSD-3-Clause
Maintainer  : dwc1295@gmail.com
-}
module Subpar.Syntax (
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

    -- ** Property tests
    syntaxTests
) where

import Data.Attoparsec.Text (
  Parser,
  char,
  choice,
  decimal,
  hexadecimal,
  many',
  many1',
  parseOnly,
  skipSpace,
  string,
  takeWhile1
  )
import Data.Bits ((.|.), shiftL)
import Data.Char (ord)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import qualified Data.Text as T
import Prelude hiding (String)
import TextShow (fromText, showt, toText)
import TextShow.Data.Integral (showbBin, showbHex)

import           Hedgehog hiding (Command)
import qualified Hedgehog.Gen   as Gen
import qualified Hedgehog.Range as Range

-- | Parse parenthesis
par :: Char -> Parser ()
par p = char p >> skipSpace

------------
-- Tokens --
------------

-- | Reserved words
newtype Reserved = Reserved{ unReserved :: Text }
  deriving (Show, Read, Eq)

reserved :: [Text]
reserved =
  [ "!"
  , "_"
  , "as"
  , "BINARY"
  , "DECIMAL"
  , "exists"
  , "HEXADECIMAL"
  , "forall"
  , "let"
  , "match"
  , "NUMERAL"
  , "par"
  , "STRING"
  , "assert"
  , "check-sat"
  , "check-sat-assuming"
  , "declare-const"
  , "declare-datatype"
  , "declare-datatypes"
  , "declare-fun"
  , "declare-sort"
  , "define-fun"
  , "define-fun-rec"
  , "define-sort"
  , "echo"
  , "exit"
  , "get-assertions"
  , "get-assignment"
  , "get-info"
  , "get-model"
  , "get-option"
  , "get-proof"
  , "get-unsat-assumptions"
  , "get-unsat-core"
  , "get-value"
  , "pop"
  , "push"
  , "reset"
  , "reset-assertions"
  , "set-info"
  , "set-logic"
  , "set-option"
  ]

-- | Parse 'Reserved'
parseReserved :: Parser Reserved
parseReserved = Reserved `fmap` choice (string <$> reserved) <* skipSpace

-- | Unparse 'Reserved'
unparseReserved :: Reserved -> Text
unparseReserved = unReserved


-- | @\<numeral\> ::= 0 | a non-empty sequence of digits not starting with 0@
newtype Numeral = Numeral Integer
  deriving (Show, Read, Eq)

-- | Parse 'Numeral'
parseNumeral :: Parser Numeral
parseNumeral = Numeral `fmap` decimal <* skipSpace -- decimal might be loose

-- | Unparse 'Numeral'
unparseNumeral :: Numeral -> Text
unparseNumeral (Numeral n) = showt n


-- | @\<decimal\> ::= \<numeral\>.0*\<numeral\>@
newtype Decimal = Decimal Double
  deriving (Show, Read, Eq)

-- | Parse 'Decimal'
parseDecimal :: Parser Decimal
parseDecimal = undefined <* skipSpace

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
parseHexadecimal = "#x" *> Hexadecimal `fmap` hexadecimal <* skipSpace

-- | Unparse 'Hexadecimal'
unparseHexadecimal :: Hexadecimal -> Text
unparseHexadecimal (Hexadecimal h) = toText $ fromText "#x" <> showbHex h

-- | 'parseHexadecimal' . 'unparseHexadecimal' == id
prop_hexadecimal_forward :: Property
prop_hexadecimal_forward = property $ do
  n <- forAll $ Gen.integral $ Range.linear 0 (maxBound :: Int)
  let hex = Hexadecimal $ fromIntegral n
  parseOnly parseHexadecimal (unparseHexadecimal hex) === Right hex


-- | @\<binary\> ::= #b followed by a non-empty sequence of 0 and 1 characters@
newtype Binary = Binary Integer
  deriving (Show, Read, Eq)

-- | Parse 'Binary'
parseBinary :: Parser Binary
parseBinary = "#b" *> Binary `fmap` binary <* skipSpace
  where
    binary = T.foldl' step 0 `fmap` takeWhile1 isBinDigit
      where
        isBinDigit c = c == '0' || c == '1'
        step a c = (a `shiftL` 1) .|. fromIntegral (ord c - 48)

-- | Unparse 'Binary'
unparseBinary :: Binary -> Text
unparseBinary (Binary b) = toText $ fromText "#b" <> showbBin b

-- | 'parseBinary' . 'unparseBinary' == id
prop_binary_forward :: Property
prop_binary_forward = property $ do
  n <- forAll $ Gen.integral $ Range.linear 0 (maxBound :: Int)
  let bin = Binary $ fromIntegral n
  parseOnly parseBinary (unparseBinary bin) === Right bin


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
parseString = undefined <* skipSpace

-- | Unparse 'String'
unparseString :: String -> Text
unparseString = undefined


{- |
@
\<simple_symbol> ::= a non-empty sequence of letters, digits, and the
characters + - / * = % ? ! . $ _ ~ ^ < > \@ that does not start with a digit.
@
-}
newtype SimpleSymbol = SimpleSymbol{ unSimpleSymbol :: Text }
  deriving (Show, Read, Eq)

-- | Parse 'SimpleSymbol'
parseSimpleSymbol :: Parser SimpleSymbol
parseSimpleSymbol = undefined <* skipSpace

-- | Unparse 'SimpleSymbol'
unparseSimpleSymbol :: SimpleSymbol -> Text
unparseSimpleSymbol = unSimpleSymbol


{- |
@
\<symbol\> ::= \<simple_symbol\>
           | a sequence of whitespace and printable characters that starts
             and ends with | and does not otherwise include | or \
@
-}
newtype Symbol = Symbol{unSymbol :: Text}
  deriving (Show, Read, Eq)

-- | Parse 'Symbol'
parseSymbol :: Parser Symbol
parseSymbol = choice
  [ undefined 
  , char '|' *> undefined <* char '|'
  ] <* skipSpace

-- | Unparse 'Symbol'
unparseSymbol :: Symbol -> Text
unparseSymbol = unSymbol


-- | @\<keyword\> ::= :\<simple_symbol\>@
newtype Keyword = Keyword{unKeyword :: Text}
  deriving (Show, Read, Eq)

-- | Parse 'Keyword'
parseKeyword :: Parser Keyword
parseKeyword = do
  _ <- char ':'
  simpleSymbol <- undefined
  skipSpace
  return $ Keyword simpleSymbol

-- | Unparse 'Keyword'
unparseKeyword :: Keyword -> Text
unparseKeyword (Keyword keyword) = ":" <> keyword


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
  , parseExprs
  ]
  where
    parseExprs = do
      par '('
      exprs <- many' parseSExpr
      par ')'
      return $ SExprs exprs

-- | Unparse 'SExpr'
unparseSExpr :: SExpr -> Text
unparseSExpr = \case
  SExprSpecConstant specConstant -> unparseSpecConstant specConstant
  SExprSymbol       symbol       -> unparseSymbol symbol
  SExprReserved     rsvd         -> unparseReserved rsvd
  SExprKeyword      keyword      -> unparseKeyword keyword
  SExprs            exprs        -> 
    T.unwords ["(", T.unwords $ unparseSExpr <$> exprs, ")"]


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
  , parseIdentifierUnderscore
  ]
  where
    parseIdentifierUnderscore = do
      par '('
      char '_' >> skipSpace
      symbol  <- parseSymbol
      indices <- NE.fromList <$> many1' parseIndex
      par ')'
      return $ IdentifierUnderscore symbol indices

-- | Unparse 'Identifier'
unparseIdentifier :: Identifier -> Text
unparseIdentifier = \case
  IdentifierSymbol sym -> unparseSymbol sym
  IdentifierUnderscore symbol indices ->
    T.unwords [ "("
              , "_"
              , unparseSymbol symbol
              , T.unwords $ unparseIndex <$> NE.toList indices
              , ")"
              ]


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
  , parseIdentifierSorts
  ]
  where
    parseIdentifierSorts = do
      par '('
      identifier <- parseIdentifier
      sorts      <- many1' parseSort
      par ')'
      return $ Sort identifier sorts

-- | Unparse 'Sort'
unparseSort :: Sort -> Text
unparseSort = \case
  Sort identifier []    -> unparseIdentifier identifier
  Sort identifier sorts -> 
    T.unwords [ "("
              , unparseIdentifier identifier
              , T.unwords $ unparseSort <$> sorts
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
  , parseAttributeValueSExprs
  ]
  where
    parseAttributeValueSExprs = do
      par '('
      exprs <- many' parseSExpr
      par ')'
      return $ AttributeValueSExprs exprs

-- | Unparse 'AttributeValue'
unparseAttributeValue :: AttributeValue -> Text
unparseAttributeValue = \case
  AttributeValueSpecConstant specConstant -> unparseSpecConstant specConstant
  AttributeValueSymbol       symbol       -> unparseSymbol symbol
  AttributeValueSExprs       exprs        ->
    T.unwords ["(", T.unwords $ unparseSExpr <$> exprs, ")"]


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
  [ AttributeKeyword               <$> parseKeyword
  , AttributeKeywordAttributeValue <$> parseKeyword <*> parseAttributeValue
  ]

-- | Unparse 'Attribute'
unparseAttribute :: Attribute -> Text
unparseAttribute = \case
  AttributeKeyword keyword -> unparseKeyword keyword
  AttributeKeywordAttributeValue keyword attributeValue ->
    T.unwords [unparseKeyword keyword, unparseAttributeValue attributeValue]


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
  , parseQualIdentifierAs
  ]
  where
    parseQualIdentifierAs = do
      par '('
      "as" *> skipSpace
      identifier <- parseIdentifier
      sort       <- parseSort
      par ')'
      return $ QualIdentifierAs identifier sort

-- | Unparse 'QualIdentifier'
unparseQualIdentifier :: QualIdentifier -> Text
unparseQualIdentifier = \case
    QualIdentifier   identifier      -> unparseIdentifier identifier
    QualIdentifierAs identifier sort -> 
      T.unwords [ "("
                , "as"
                , unparseIdentifier identifier
                , unparseSort sort
                , ")"
                ]


-- | @\<var_binding\> ::= ( \<symbol\> \<term\> )@
data VarBinding = VarBinding Symbol Term
    deriving (Show, Read, Eq)

-- | Parse 'VarBinding'
parseVarBinding :: Parser VarBinding
parseVarBinding = do
  par '('
  symbol <- parseSymbol
  term   <- parseTerm
  par ')'
  return $ VarBinding symbol term

-- | Unparse 'VarBinding'
unparseVarBinding :: VarBinding -> Text
unparseVarBinding (VarBinding symbol term) =
    T.unwords ["(", unparseSymbol symbol, unparseTerm term, ")"]


-- | @\<sorted_var\> ::= ( \<symbol\> \<sort\> )@
data SortedVar = SortedVar Symbol Sort
  deriving (Show, Read, Eq)

-- | Parse 'SortedVar'
parseSortedVar :: Parser SortedVar
parseSortedVar = do
  par '('
  symbol <- parseSymbol
  sort   <- parseSort
  par ')'
  return $ SortedVar symbol sort

-- | Unparse 'SortedVar'
unparseSortedVar :: SortedVar -> Text
unparseSortedVar (SortedVar symbol sort) = 
  T.unwords ["(", unparseSymbol symbol, unparseSort sort, ")"]


-- | @\<pattern\> ::= \<symbol\> | ( \<symbol\> \<symbol\>+ )@
data Pattern
  = -- | \<symbol\>
    Pattern Symbol
  | -- | ( \<symbol\> \<symbol\>+ )
    Patterns Symbol (NonEmpty Symbol)
  deriving (Show, Read, Eq)

-- | Parse 'Pattern'
parsePattern :: Parser Pattern
parsePattern = choice
  [ Pattern <$> parseSymbol
  , parsePatterns
  ]
  where
    parsePatterns = do
      par '('
      symbol  <- parseSymbol
      symbols <- many1' parseSymbol
      par ')'
      return $ Patterns symbol $ NE.fromList symbols

-- | Unparse 'Pattern'
unparsePattern :: Pattern -> Text
unparsePattern = \case
  Pattern  symbol         -> unparseSymbol symbol
  Patterns symbol symbols -> 
    T.unwords [ "("
              , unparseSymbol symbol
              , T.unwords $ unparseSymbol <$> NE.toList symbols
              , ")"
              ]


-- | @\<match_case\> ::= ( \<pattern\> \<term\> )@
data MatchCase = MatchCase Pattern Term
    deriving (Show, Read, Eq)

-- | Parse 'MatchCase'
parseMatchCase :: Parser MatchCase
parseMatchCase = do
  par '('
  patt <- parsePattern
  term <- parseTerm
  par ')'
  return $ MatchCase patt term

-- | Unparse 'MatchCase'
unparseMatchCase :: MatchCase -> Text
unparseMatchCase (MatchCase patt term) =
  T.unwords ["(", unparsePattern patt, unparseTerm term, ")"]


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
  , parseTermQualIdentifiers
  , parseTermLet
  , parseTermForall
  , parseTermExists
  , parseTermMatch
  , parseTermExclamation
  ]
  where
    parseTermQualIdentifiers = do
      par '('
      qualIdentifier <- parseQualIdentifier
      terms          <- NE.fromList <$> many1' parseTerm
      par ')'
      return $ TermQualIdentifiers qualIdentifier terms
    parseTermLet = do
      par '('
      "let" *> skipSpace
      par '('
      varBindings <- NE.fromList <$> many1' parseVarBinding
      par ')'
      term        <- parseTerm
      par ')'
      return $ TermLet varBindings term
    parseTermForall = do
      par '('
      "forall" *> skipSpace
      par '('
      sortedVars <- NE.fromList <$> many1' parseSortedVar
      par ')'
      term       <- parseTerm
      par ')'
      return $ TermForall sortedVars term
    parseTermExists = do
      par '('
      "exists" *> skipSpace
      par '('
      sortedVars <- NE.fromList <$> many1' parseSortedVar
      par ')'
      term       <- parseTerm
      par ')'
      return $ TermExists sortedVars term
    parseTermMatch = do
      par '('
      "match" *> skipSpace
      term       <- parseTerm
      par '('
      matchCases <- NE.fromList <$> many1' parseMatchCase
      par ')'
      par ')'
      return $ TermMatch term matchCases
    parseTermExclamation = do
      par '('
      char '!' *> skipSpace
      term       <- parseTerm
      attributes <- NE.fromList <$> many1' parseAttribute
      par ')'
      return $ TermExclamation term attributes

-- | Unparse 'Term'
unparseTerm :: Term -> Text
unparseTerm = \case
  TermSpecConstant specConstant -> unparseSpecConstant specConstant
  TermQualIdentifier qualIdentifier -> unparseQualIdentifier qualIdentifier
  TermQualIdentifiers qualIdentifier terms ->
    T.unwords 
      [ "("
      , unparseQualIdentifier qualIdentifier
      , T.unwords $ unparseTerm <$> NE.toList terms
      , ")"
      ]
  TermLet varBindings term -> 
    T.unwords 
      [ "("
      , "let"
      , "("
      , T.unwords $ unparseVarBinding <$> NE.toList varBindings
      , ")"
      , unparseTerm term
      , ")"
      ]
  TermForall sortedVars term -> 
    T.unwords 
      [ "("
      , "forall"
      , "("
      , T.unwords $ unparseSortedVar <$> NE.toList sortedVars
      , ")"
      , unparseTerm term
      , ")"
      ]
  TermExists sortedVars term ->
    T.unwords
      [ "("
      , "exists"
      , "("
      , T.unwords $ unparseSortedVar <$> NE.toList sortedVars
      , ")"
      , unparseTerm term
      , ")"
      ]
  TermMatch term matchCases ->
    T.unwords
      [ "("
      , "match"
      , unparseTerm term
      , "("
      , T.unwords $ unparseMatchCase <$> NE.toList matchCases
      , ")"
      , ")"
      ]
  TermExclamation term attributes ->
    T.unwords
      [ "("
      , "!"
      , unparseTerm term
      , T.unwords $ unparseAttribute <$> NE.toList attributes
      , ")"
      ]


--------------
-- Theories --
--------------

-- | @\<sort_symbol_decl\> ::= ( \<identifier\> \<numeral\> \<attribute\>* )@
data SortSymbolDecl = SortSymbolDecl Identifier Numeral [Attribute]
  deriving (Show, Read, Eq)

-- | Parse 'SortSymbolDecl'
parseSortSymbolDecl :: Parser SortSymbolDecl
parseSortSymbolDecl = do
  par '('
  identifier <- parseIdentifier
  numeral    <- parseNumeral
  attributes <- many' parseAttribute
  par ')'
  return $ SortSymbolDecl identifier numeral attributes

-- | Unparse 'SortSymbolDecl'
unparseSortSymbolDecl :: SortSymbolDecl -> Text
unparseSortSymbolDecl (SortSymbolDecl identifier numeral attributes) =
  T.unwords [ "("
            , unparseIdentifier identifier
            , unparseNumeral numeral
            , T.unwords $ unparseAttribute <$> attributes
            , ")"
            ]


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
  ] <* skipSpace

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
parseFunSymbolDecl = choice
  [ parseFunSymbolDeclSpecConstant
  , parseFunSymbolDeclMetaSpecConstant
  , parseFunSymbolDeclIdentifier
  ]
  where
    parseFunSymbolDeclSpecConstant = do
      par '('
      specConstant <- parseSpecConstant
      sort         <- parseSort
      attributes   <- many' parseAttribute
      par ')'
      return $ FunSymbolDeclSpecConstant specConstant sort attributes
    parseFunSymbolDeclMetaSpecConstant = do
      par '('
      metaSpecConstant <- parseMetaSpecConstant
      sort             <- parseSort
      attributes       <- many' parseAttribute
      par ')'
      return $ FunSymbolDeclMetaSpecConstant metaSpecConstant sort attributes
    parseFunSymbolDeclIdentifier = do
      par '('
      identifier <- parseIdentifier
      sorts      <- NE.fromList <$> many1' parseSort
      attributes <- many' parseAttribute
      par ')'
      return $ FunSymbolDeclIdentifier identifier sorts attributes

-- | Unparse 'FunSymbolDecl'
unparseFunSymbolDecl :: FunSymbolDecl -> Text
unparseFunSymbolDecl = \case
  FunSymbolDeclSpecConstant specConstant sort attributes ->
    T.unwords
      [ "("
      , unparseSpecConstant specConstant
      , unparseSort sort
      , T.unwords $ unparseAttribute <$> attributes
      , ")"
      ]
  FunSymbolDeclMetaSpecConstant metaSpecConstant sort attributes ->
    T.unwords
      [ "("
      , unparseMetaSpecConstant metaSpecConstant
      , unparseSort sort
      , T.unwords $ unparseAttribute <$> attributes
      , ")"
      ]
  FunSymbolDeclIdentifier identifier sorts attributes ->
    T.unwords
      [ "("
      , unparseIdentifier identifier
      , T.unwords $ unparseSort <$> NE.toList sorts
      , T.unwords $ unparseAttribute <$> attributes
      , ")"
      ]


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
  , parsePar
  ]
  where
    parsePar = do
      par '('
      "par" *> skipSpace
      par '('
      symbols    <- NE.fromList <$> many1' parseSymbol
      par ')'
      par '('
      identifier <- parseIdentifier
      sorts      <- NE.fromList <$> many1' parseSort
      attributes <- many' parseAttribute
      par ')'
      par ')'
      return $ Par symbols identifier sorts attributes

-- | Unparse 'ParFunSymbolDecl'
unparseParFunSymbolDecl :: ParFunSymbolDecl -> Text
unparseParFunSymbolDecl = \case
  ParFunSymbolDeclFunSymbolDecl funSymbolDecl ->
    unparseFunSymbolDecl funSymbolDecl
  Par symbols identifier sorts attributes ->
    T.unwords
      [ "("
      , "par"
      , "("
      , T.unwords $ unparseSymbol <$> NE.toList symbols
      , ")"
      , "("
      , unparseIdentifier identifier
      , T.unwords $ unparseSort <$> NE.toList sorts
      , T.unwords $ unparseAttribute <$> attributes
      , ")"
      , ")"
      ]


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
parseTheoryAttribute = choice
  [ parseTheoryAttributeSorts
  , parseTheoryAttributeFuns
  , parseTheoryAttributeSortsDescription
  , parseTheoryAttributeFunsDescription
  , ":definition" *> skipSpace >> TheoryAttributeDefinition <$> parseString
  , ":values"     *> skipSpace >> TheoryAttributeValues     <$> parseString
  , ":notes"      *> skipSpace >> TheoryAttributeNotes      <$> parseString
  , TheoryAttributeAttribute <$> parseAttribute
  ]
  where
    parseTheoryAttributeSorts = do
      ":sorts" *> skipSpace
      par '('
      sortSymbolDecls <- NE.fromList <$> many1' parseSortSymbolDecl
      par ')'
      return $ TheoryAttributeSorts sortSymbolDecls
    parseTheoryAttributeFuns = do
      ":funs" *> skipSpace
      par '('
      parFunSymbolDecls <- NE.fromList <$> many1' parseParFunSymbolDecl
      par ')'
      return $ TheoryAttributeFuns parFunSymbolDecls
    parseTheoryAttributeSortsDescription = do
      ":sorts-description" *> skipSpace
      TheoryAttributeSortsDescription <$> parseString
    parseTheoryAttributeFunsDescription = do
      ":funs-description" *> skipSpace
      TheoryAttributeFunsDescription <$> parseString

-- | Unparse 'TheoryAttribute'
unparseTheoryAttribute :: TheoryAttribute -> Text
unparseTheoryAttribute = \case
  TheoryAttributeSorts sortSymbolDecls ->
    T.unwords
      [ ":sorts"
      , "("
      , T.unwords $ unparseSortSymbolDecl <$> NE.toList sortSymbolDecls
      , ")"
      ]
  TheoryAttributeFuns parFunSymbolDecls ->
    T.unwords
      [ ":funs"
      , "("
      , T.unwords $ unparseParFunSymbolDecl <$> NE.toList parFunSymbolDecls
      , ")"
      ]
  TheoryAttributeSortsDescription str ->
    T.unwords [":sorts-description", unparseString str]
  TheoryAttributeFunsDescription str ->
    T.unwords [":funs-description", unparseString str]
  TheoryAttributeDefinition str ->
    T.unwords [":definition", unparseString str]
  TheoryAttributeValues str ->
    T.unwords [":values", unparseString str]
  TheoryAttributeNotes str ->
    T.unwords [":notes", unparseString str]
  TheoryAttributeAttribute attribute -> unparseAttribute attribute


-- | @\<theory_decl\> ::= ( theory \<symbol\> \<theory_attribute\>+ )@
data TheoryDecl = TheoryDecl Symbol (NonEmpty TheoryAttribute)
  deriving (Show, Read, Eq)

-- | Parse 'TheoryDecl'
parseTheoryDecl :: Parser TheoryDecl
parseTheoryDecl = do
  par '('
  "theory" *> skipSpace
  symbol           <- parseSymbol
  theoryAttributes <- NE.fromList <$> many1' parseTheoryAttribute
  par ')'
  return $ TheoryDecl symbol theoryAttributes

-- | Unparse 'TheoryDecl'
unparseTheoryDecl :: TheoryDecl -> Text
unparseTheoryDecl (TheoryDecl symbol theoryAttributes) = T.unwords
  [ "("
  , "theory"
  , unparseSymbol symbol
  , T.unwords $ unparseTheoryAttribute <$> NE.toList theoryAttributes
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
parseLogicAttribute = choice
  [ parseLogicAttributeTheories
  , ":language"   *> skipSpace >> LogicAttributeLanguage   <$> parseString
  , ":extensions" *> skipSpace >> LogicAttributeExtensions <$> parseString
  , ":values"     *> skipSpace >> LogicAttributeValues     <$> parseString
  , ":notes"      *> skipSpace >> LogicAttributeNotes      <$> parseString
  , LogicAttributeAttribute <$> parseAttribute
  ]
  where
    parseLogicAttributeTheories = do
      ":theories" *> skipSpace
      par '('
      symbols <- NE.fromList <$> many1' parseSymbol
      par ')'
      return $ LogicAttributeTheories symbols

-- | Unparse 'LogicAttribute'
unparseLogicAttribute :: LogicAttribute -> Text
unparseLogicAttribute = \case
  LogicAttributeTheories symbols ->
    T.unwords
      [ ":theories"
      , "("
      , T.unwords $ unparseSymbol <$> NE.toList symbols
      , ")"
      ]
  LogicAttributeLanguage   str -> T.unwords [":language"  , unparseString str]
  LogicAttributeExtensions str -> T.unwords [":extensions", unparseString str]
  LogicAttributeValues     str -> T.unwords [":values"    , unparseString str]
  LogicAttributeNotes      str -> T.unwords [":notes"     , unparseString str]
  LogicAttributeAttribute attribute -> unparseAttribute attribute


-- | @\<logic\> ::= ( logic \<symbol\> \<logic_attribute\>+ )@
data Logic = Logic Symbol (NonEmpty LogicAttribute)
  deriving (Show, Read, Eq)

-- | Parse 'Logic'
parseLogic :: Parser Logic
parseLogic = do
  par '('
  "logic" *> skipSpace
  symbol          <- parseSymbol
  logicAttributes <- NE.fromList <$> many1' parseLogicAttribute
  par ')'
  return $ Logic symbol logicAttributes

-- | Unparse 'Logic'
unparseLogic :: Logic -> Text
unparseLogic (Logic symbol logicAttributes) = T.unwords
  [ "("
  , "logic"
  , unparseSymbol symbol
  , T.unwords $ unparseLogicAttribute <$> NE.toList logicAttributes
  , ")"
  ]


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
  ] <* skipSpace

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
newtype BValue = BValue Bool
  deriving (Show, Read, Eq)

-- | Parse 'BValue'
parseBValue :: Parser BValue
parseBValue = choice 
  [ BValue True  <$ string "true"
  , BValue False <$ string "false"
  ] <* skipSpace

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
  [ ":diagnostic-output-channel"       *>
      OptionDiagnosticOutputChannel   `fmap` parseString
  , ":global-declarations"             *>
      OptionGlobalDeclarations        `fmap` parseBValue
  , ":interactive-mode"                *>
      OptionInteractiveMode           `fmap` parseBValue
  , ":print-success"                   *>
      OptionPrintSuccess              `fmap` parseBValue
  , ":produce-assertions"              *>
      OptionProduceAssertions         `fmap` parseBValue
  , ":produce-assignments"             *>
      OptionProduceAssignments        `fmap` parseBValue
  , ":produce-models"                  *>
      OptionProduceModels             `fmap` parseBValue
  , ":produce-proofs"                  *>
      OptionProduceProofs             `fmap` parseBValue
  , ":produce-unsat-assumptions"       *>
      OptionProduceUnsatAssumptions   `fmap` parseBValue
  , ":produce-unsat-cores"             *>
      OptionProduceUnsatCores         `fmap` parseBValue
  , ":random-seed"                     *>
      OptionRandomSeed                `fmap` parseNumeral
  , ":regular-output-channel"          *>
      OptionRegularOutputChannel      `fmap` parseString
  , ":reproducible-resource-limit"     *>
      OptionReproducibleResourceLimit `fmap` parseNumeral
  , ":verbosity"                       *>
      OptionVerbosity                  `fmap` parseNumeral
  , OptionAttribute                   <$> parseAttribute
  ]

-- | Unparse 'Option'
unparseOption :: Option -> Text
unparseOption = \case
  OptionDiagnosticOutputChannel   str ->
    T.unwords [":diagnostic-output-channel"  , unparseString  str]
  OptionGlobalDeclarations        b   ->
    T.unwords [":global-declarations"        , unparseBValue  b  ]
  OptionInteractiveMode           b   ->
    T.unwords [":interactive-mode"           , unparseBValue  b  ]
  OptionPrintSuccess              b   ->
    T.unwords [":print-success"              , unparseBValue  b  ]
  OptionProduceAssertions         b   ->
    T.unwords [":produce-assertions"         , unparseBValue  b  ]
  OptionProduceAssignments        b   ->
    T.unwords [":produce-assignments"        , unparseBValue  b  ]
  OptionProduceModels             b   ->
    T.unwords [":produce-models"             , unparseBValue  b  ]
  OptionProduceProofs             b   ->
    T.unwords [":produce-proofs"             , unparseBValue  b  ]
  OptionProduceUnsatAssumptions   b   ->
    T.unwords [":produce-unsat-assumptions"  , unparseBValue  b  ]
  OptionProduceUnsatCores         b   ->
    T.unwords [":produce-unsat-cores"        , unparseBValue  b  ]
  OptionRandomSeed                n   ->
    T.unwords [":random-seed"                , unparseNumeral n  ]
  OptionRegularOutputChannel      str ->
    T.unwords [":regular-output-channel"     , unparseString  str]
  OptionReproducibleResourceLimit n   ->
    T.unwords [":reproducible-resource-limit", unparseNumeral n  ]
  OptionVerbosity                 n   ->
    T.unwords [":verbosity"                  , unparseNumeral n  ]
  OptionAttribute attribute -> unparseAttribute attribute

--------------
-- Commands --
--------------

-- | @\<sort_dec\> ::= ( \<symbol\> \<numeral\> )@
data SortDec = SortDec Symbol Numeral
    deriving (Show, Read, Eq)

-- | Parse 'SortDec'
parseSortDec :: Parser SortDec
parseSortDec = do
  par '('
  symbol  <- parseSymbol
  numeral <- parseNumeral
  par ')'
  return $ SortDec symbol numeral

-- | Unparse 'SortDec'
unparseSortDec :: SortDec -> Text
unparseSortDec (SortDec symbol numeral) =
  T.unwords ["(", unparseSymbol symbol, unparseNumeral numeral, ")"]


-- | @\<selector_dec\> ::= ( \<symbol\> \<sort\> )@
data SelectorDec = SelectorDec Symbol Sort
    deriving (Show, Read, Eq)

-- | Parse 'SelectorDec'
parseSelectorDec :: Parser SelectorDec
parseSelectorDec = do
  par '('
  symbol <- parseSymbol
  sort   <- parseSort
  par ')'
  return $ SelectorDec symbol sort

-- | Unparse 'SelectorDec'
unparseSelectorDec :: SelectorDec -> Text
unparseSelectorDec (SelectorDec symbol sort) =
  T.unwords ["(", unparseSymbol symbol, unparseSort sort, ")"]


-- | @\<constructor_dec\> ::= ( \<symbol\> \<selector_dec\>* )@
data ConstructorDec = ConstructorDec Symbol [SelectorDec]
    deriving (Show, Read, Eq)

-- | Parse 'ConstructorDec'
parseConstructorDec :: Parser ConstructorDec
parseConstructorDec = do
  par '('
  symbol       <- parseSymbol
  selectorDecs <- many' parseSelectorDec
  par ')'
  return $ ConstructorDec symbol selectorDecs

-- | Unparse 'ConstructorDec'
unparseConstructorDec :: ConstructorDec -> Text
unparseConstructorDec (ConstructorDec symbol selectorDecs) = T.unwords 
  [ "("
  , unparseSymbol symbol
  , T.unwords $ unparseSelectorDec <$> selectorDecs
  , ")"
  ]


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
parseDatatypeDec = choice
  [ parseDatatypeDec'
  , parseDatatypeDecPar
  ]
  where
    parseDatatypeDec' = do
      par '('
      constructorDecs <- NE.fromList <$> many1' parseConstructorDec
      par ')'
      return $ DatatypeDec constructorDecs
    parseDatatypeDecPar = do
      par '('
      "par" *> skipSpace
      par '('
      symbols         <- NE.fromList <$> many1' parseSymbol
      par ')'
      par '('
      constructorDecs <- NE.fromList <$> many1' parseConstructorDec
      par ')'
      par ')'
      return $ DatatypeDecPar symbols constructorDecs

-- | Unparse 'DatatypeDec'
unparseDatatypeDec :: DatatypeDec -> Text
unparseDatatypeDec = \case
  DatatypeDec constructorDecs ->
    T.unwords 
      [ "("
      , T.unwords $ unparseConstructorDec <$> NE.toList constructorDecs
      , ")"
      ]
  DatatypeDecPar symbols constructorDecs ->
    T.unwords
      [ "("
      , "par"
      , "("
      , T.unwords $ unparseSymbol <$> NE.toList symbols
      , ")"
      , "("
      , T.unwords $ unparseConstructorDec <$> NE.toList constructorDecs
      , ")"
      , ")"
      ]


-- | @\<function_dec\> ::= ( \<symbol\> ( \<sorted_var\>* ) \<sort\> )@
data FunctionDec = FunctionDec Symbol [SortedVar] Sort
    deriving (Show, Read, Eq)

-- | Parse 'FunctionDec'
parseFunctionDec :: Parser FunctionDec
parseFunctionDec = do
  par '('
  symbol     <- parseSymbol
  par '('
  sortedVars <- many' parseSortedVar
  par ')'
  sort       <- parseSort
  par ')'
  return $ FunctionDec symbol sortedVars sort

-- | Unparse 'FunctionDec'
unparseFunctionDec :: FunctionDec -> Text
unparseFunctionDec (FunctionDec symbol sortedVars sort) = T.unwords
  [ "("
  , unparseSymbol symbol
  , "("
  , T.unwords $ unparseSortedVar <$> sortedVars
  , ")"
  , unparseSort sort
  , ")"
  ]


-- | @\<function_def\> ::= \<symbol\> ( \<sorted_var\>* ) \<sort\> \<term\>@
data FunctionDef = FunctionDef Symbol [SortedVar] Sort Term
    deriving (Show, Read, Eq)

-- | Parse 'FunctionDef'
parseFunctionDef :: Parser FunctionDef
parseFunctionDef = do
  symbol     <- parseSymbol
  par '('
  sortedVars <- many' parseSortedVar
  par ')'
  FunctionDef symbol sortedVars <$> parseSort <*> parseTerm

unparseFunctionDef :: FunctionDef -> Text
unparseFunctionDef (FunctionDef symbol sortedVars sort term) = T.unwords 
  [ unparseSymbol symbol
  , "("
  , T.unwords $ unparseSortedVar <$> sortedVars
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
  [ PropLiteralSymbol <$> parseSymbol
  , parsePropLiteralNotSymbol
  ]
  where
    parsePropLiteralNotSymbol = do
      par '('
      "not" *> skipSpace
      symbol <- parseSymbol
      par ')'
      return $ PropLiteralNotSymbol symbol

-- | Unparse 'PropLiteral'
unparsePropLiteral :: PropLiteral -> Text
unparsePropLiteral = \case
  PropLiteralSymbol symbol -> unparseSymbol symbol
  PropLiteralNotSymbol symbol ->
    T.unwords ["(", "not", unparseSymbol symbol, ")"]


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
parseCommand = choice
  [ parseAssert
  , parseCheckSat
  , parseCheckSatAssuming
  , parseDeclareConst
  , parseDeclareDatatype
  , parseDeclareDatatypes
  , parseDeclareFun
  , parseDeclareSort
  , parseDefineFun
  , parseDefineFunRec
  , parseDefineFunsRec
  , parseDefineSort
  , parseEcho
  , parseExit
  , parseGetAssertions
  , parseGetAssignment
  , parseGetInfo
  , parseGetModel
  , parseGetOption
  , parseGetProof
  , parseGetUnsatAssumptions
  , parseGetUnsatCore
  , parseGetValue
  , parsePop
  , parsePush
  , parseReset
  , parseResetAssertions
  , parseSetInfo
  , parseSetLogic
  , parseSetOption
  ]
  where
    parseAssert = do
      par '('
      "assert" *> skipSpace
      term <- parseTerm
      par ')'
      return $ Assert term
    parseCheckSat = do
      par '('
      "check-sat" *> skipSpace
      par ')'
      return CheckSat
    parseCheckSatAssuming = do
      par '('
      "check-sat-assuming" *> skipSpace
      par '('
      propLiterals <- many' parsePropLiteral
      par ')'
      par ')'
      return $ CheckSatAssuming propLiterals
    parseDeclareConst = do
      par '('
      "declare-const" *> skipSpace
      symbol <- parseSymbol
      sort   <- parseSort
      par ')'
      return $ DeclareConst symbol sort
    parseDeclareDatatype = do
      par '('
      "declare-datatype" *> skipSpace
      symbol      <- parseSymbol
      datatypeDec <- parseDatatypeDec
      par ')'
      return $ DeclareDatatype symbol datatypeDec
    parseDeclareDatatypes = do
      par '('
      "declare-datatypes" *> skipSpace
      par '('
      sortDecs     <- NE.fromList <$> many1' parseSortDec
      par ')'
      par '('
      datatypeDecs <- NE.fromList <$> many1' parseDatatypeDec
      par ')'
      par ')'
      return $ DeclareDatatypes $ NE.zip sortDecs datatypeDecs
    parseDeclareFun = do
      par '('
      "declare-fun" *> skipSpace
      symbol <- parseSymbol
      par '('
      sorts  <- many' parseSort
      par ')'
      sort   <- parseSort
      par ')'
      return $ DeclareFun symbol sorts sort
    parseDeclareSort = do
      par '('
      "declare-sort" *> skipSpace
      symbol  <- parseSymbol
      numeral <- parseNumeral
      par ')'
      return $ DeclareSort symbol numeral
    parseDefineFun = do
      par '('
      "define-fun" *> skipSpace
      functionDef <- parseFunctionDef
      par ')'
      return $ DefineFun functionDef
    parseDefineFunRec = do
      par '('
      "define-fun-rec" *> skipSpace
      functionDef <- parseFunctionDef
      par ')'
      return $ DefineFunRec functionDef
    parseDefineFunsRec = do
      par '('
      "define-funs-rec" *> skipSpace
      par '('
      functionDecs <- NE.fromList <$> many1' parseFunctionDec
      par ')'
      par '('
      terms        <- NE.fromList <$> many1' parseTerm
      par ')'
      par ')'
      return $ DefineFunsRec $ NE.zip functionDecs terms
    parseDefineSort = do
      par '('
      "define-sort" *> skipSpace
      symbol <- parseSymbol
      par '('
      symbols <- many' parseSymbol
      par ')'
      sort <- parseSort
      par ')'
      return $ DefineSort symbol symbols sort
    parseEcho = do
      par '('
      "echo" *> skipSpace
      str <- parseString
      par ')'
      return $ Echo str
    parseExit = do
      par '('
      "exit" *> skipSpace
      par ')'
      return Exit
    parseGetAssertions = do
      par '('
      "get-assertions" *> skipSpace
      par ')'
      return GetAssertions
    parseGetAssignment = do
      par '('
      "get-assignment" *> skipSpace
      par ')'
      return GetAssignment
    parseGetInfo = do
      par '('
      "get-info" *> skipSpace
      infoFlag <- parseInfoFlag
      par ')'
      return $ GetInfo infoFlag
    parseGetModel = do
      par '('
      "get-model" *> skipSpace
      par ')'
      return GetModel
    parseGetOption = do
      par '('
      "get-option" *> skipSpace
      keyword <- parseKeyword
      par ')'
      return $ GetOption keyword
    parseGetProof = do
      par '('
      "get-proof" *> skipSpace
      par ')'
      return GetProof
    parseGetUnsatAssumptions = do
      par '('
      "get-unsat-assumptions" *> skipSpace
      par ')'
      return GetUnsatAssumptions
    parseGetUnsatCore = do
      par '('
      "get-unsat-core" *> skipSpace
      par ')'
      return GetUnsatCore
    parseGetValue = do
      par '('
      "get-value" *> skipSpace
      par '('
      terms <- NE.fromList <$> many1' parseTerm
      par ')'
      par ')'
      return $ GetValue terms
    parsePop = do
      par '('
      "pop" *> skipSpace
      num <- parseNumeral
      par ')'
      return $ Pop num
    parsePush = do
      par '('
      "push" *> skipSpace
      num <- parseNumeral
      par ')'
      return $ Push num
    parseReset = do
      par '('
      "reset" *> skipSpace
      par ')'
      return Reset
    parseResetAssertions = do
      par '('
      "reset_assertions" *> skipSpace
      par ')'
      return ResetAssertions
    parseSetInfo = do
      par '('
      "set-info" *> skipSpace
      attribute <- parseAttribute
      par ')'
      return $ SetInfo attribute
    parseSetLogic = do
      par '('
      "set-logic" *> skipSpace
      symbol <- parseSymbol
      par ')'
      return $ SetLogic symbol
    parseSetOption = do
      par '('
      "set-option" *> skipSpace
      option <- parseOption
      par ')'
      return $ SetOption option

-- | Unparse 'Command'
unparseCommand :: Command -> Text
unparseCommand = \case
  Assert term -> T.unwords ["(", "assert", unparseTerm term, ")"]
  CheckSat -> "( check-sat )"
  CheckSatAssuming propLiterals ->
    T.unwords
      [ "("
      , "check-sat-assuming"
      , "("
      , T.unwords $ unparsePropLiteral <$> propLiterals
      , ")"
      , ")"
      ]
  DeclareConst symbol sort ->
    T.unwords 
      [ "("
      , "declare-const"
      , unparseSymbol symbol
      , unparseSort sort
      , ")"
      ]
  DeclareDatatype symbol datatypeDec ->
    T.unwords
      [ "("
      , "declare-datatype"
      , unparseSymbol symbol
      , unparseDatatypeDec datatypeDec
      , ")"
      ]
  DeclareDatatypes sortDecsDatatypeDecs ->
    T.unwords
      [ "("
      , "declare-datatypes"
      , "("
      , T.unwords $ unparseSortDec     . fst <$> NE.toList sortDecsDatatypeDecs
      , ")"
      , "("
      , T.unwords $ unparseDatatypeDec . snd <$> NE.toList sortDecsDatatypeDecs
      , ")"
      , ")"
      ]
  DeclareFun symbol sorts sort ->
    T.unwords
      [ "("
      , "declare-fun"
      , unparseSymbol symbol
      , "("
      , T.unwords $ unparseSort <$> sorts
      , ")"
      , unparseSort sort
      , ")"
      ]
  DeclareSort symbol numeral ->
    T.unwords
      [ "("
      , "declare-sort"
      , unparseSymbol symbol
      , unparseNumeral numeral
      , ")"
      ]
  DefineFun functionDef -> 
    T.unwords ["(", "define-fun", unparseFunctionDef functionDef, ")"]
  DefineFunRec functionDef ->
    T.unwords ["(", "define-fun-rec", unparseFunctionDef functionDef, ")"]
  DefineFunsRec functionDecsTerms ->
    T.unwords
      [ "("
      , "define-funs-rec"
      , "("
      , T.unwords $ unparseFunctionDec . fst <$> NE.toList functionDecsTerms
      , ")"
      , "("
      , T.unwords $ unparseTerm . snd <$> NE.toList functionDecsTerms
      , ")"
      , ")"
      ]
  DefineSort symbol symbols sort ->
    T.unwords
      [ "("
      , "define-sort"
      , unparseSymbol symbol
      , "("
      , T.unwords $ unparseSymbol <$> symbols
      , ")"
      , unparseSort sort
      , ")"
      ]
  Echo str -> T.unwords ["(", "echo", unparseString str, ")"]
  Exit -> "( exit )"
  GetAssertions -> "( get-assertions )"
  GetAssignment -> "( get-assignment )"
  GetInfo infoFlag ->
    T.unwords ["(", "get-info", unparseInfoFlag infoFlag, ")"]
  GetModel -> "( get-model )"
  GetOption keyword ->
    T.unwords ["(", "get-option", unparseKeyword keyword, ")"]
  GetProof -> "( get-proof )"
  GetUnsatAssumptions -> "( get-unsat-assumptions )"
  GetUnsatCore -> "( get-unsat-core )"
  GetValue terms ->
    T.unwords
      [ "("
      , "get-value"
      , "("
      , T.unwords $ unparseTerm <$> NE.toList terms
      , ")"
      , ")"
      ]
  Pop  num -> T.unwords ["(", "pop",  unparseNumeral num, ")"]
  Push num -> T.unwords ["(", "push", unparseNumeral num, ")"]
  Reset -> "( reset )"
  ResetAssertions -> "( reset-assertions )"
  SetInfo attribute ->
    T.unwords ["(", "set-info", unparseAttribute attribute, ")"]
  SetLogic symbol ->
    T.unwords ["(", "set-logic", unparseSymbol symbol, ")"]
  SetOption option ->
    T.unwords ["(", "set-option", unparseOption option, ")"]


-- | @\<script\> ::= \<command\>*@
newtype Script = Script{ unScript :: [Command] }
  deriving (Show, Read, Eq)

-- | Parse 'Script'
parseScript :: Parser Script
parseScript = Script <$> many' parseCommand

-- | Unparse 'Script'
unparseScript :: Script -> Text
unparseScript (Script commands) = T.unlines $ unparseCommand <$> commands


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
  ] <* skipSpace

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
  ] <* skipSpace

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
parseModelResponse = choice
  [ parseModelResponseDefineFun
  , parseModelResponseDefineFunRec
  , parseModelResponseDefineFunsRec
  ]
  where
    parseModelResponseDefineFun = do
      par '('
      "define-fun" *> skipSpace
      functionDef <- parseFunctionDef
      par ')'
      return $ ModelResponseDefineFun functionDef
    parseModelResponseDefineFunRec = do
      par '('
      "define-fun-rec" *> skipSpace
      functionDef <- parseFunctionDef
      par ')'
      return $ ModelResponseDefineFunRec functionDef
    parseModelResponseDefineFunsRec = do
      par '('
      "define-funs-rec" *> skipSpace
      par '('
      functionDecs <- NE.fromList <$> many1' parseFunctionDec
      par ')'
      par '('
      terms        <- NE.fromList <$> many1' parseTerm
      par ')'
      par ')'
      return $ ModelResponseDefineFunsRec $ NE.zip functionDecs terms

-- | Unparse 'ModelResponse'
unparseModelResponse :: ModelResponse -> Text
unparseModelResponse = \case
  ModelResponseDefineFun functionDef ->
    T.unwords ["(", "define-fun", unparseFunctionDef functionDef, ")"]
  ModelResponseDefineFunRec functionDef ->
    T.unwords ["(", "define-fun-rec", unparseFunctionDef functionDef, ")"]
  ModelResponseDefineFunsRec functionDecsTerms ->
    T.unwords 
      [ "("
      , "define-funs-rec"
      , "("
      , T.unwords $ unparseFunctionDec . fst <$> NE.toList functionDecsTerms
      , ")"
      , "("
      , T.unwords $ unparseTerm . snd <$> NE.toList functionDecsTerms
      , ")"
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
  [ parseInfoResponseAssertionStackLevels
  , ":authors" *> skipSpace >> InfoResponseAuthors <$> parseString
  , parseInfoResponseErrorBehavior
  , ":name" *> skipSpace >> InfoResponseName <$> parseString
  , parseInfoResponseReasonUnknown
  , ":version" *> skipSpace >> InfoResponseVersion <$> parseString
  , InfoResponseAttribute <$> parseAttribute
  ]
  where
    parseInfoResponseAssertionStackLevels = do
      ":assertion-stack-levels" *> skipSpace
      InfoResponseAssertionStackLevels <$> parseNumeral
    parseInfoResponseErrorBehavior = do
      ":error-behavior" *> skipSpace
      InfoResponseErrorBehavior <$> parseErrorBehavior
    parseInfoResponseReasonUnknown = do
      ":reason-unknown" *> skipSpace
      InfoResponseReasonUnknown <$> parseReasonUnknown

-- | Unparse 'InfoResponse'
unparseInfoResponse :: InfoResponse -> Text
unparseInfoResponse = \case
  InfoResponseAssertionStackLevels num ->
      T.unwords [":assertion-stack-levels", unparseNumeral num]
  InfoResponseAuthors str ->
      T.unwords [":authors", unparseString str]
  InfoResponseErrorBehavior errorBehavior ->
      T.unwords [":error-behavior", unparseErrorBehavior errorBehavior]
  InfoResponseName str ->
      T.unwords [":name", unparseString str]
  InfoResponseReasonUnknown reasonUnknown ->
      T.unwords [":reason-unknown", unparseReasonUnknown reasonUnknown]
  InfoResponseVersion str -> 
      T.unwords [":version", unparseString str]
  InfoResponseAttribute attribute ->
      unparseAttribute attribute


-- | @\<valuation_pair\> ::= ( \<term\> \<term\> )@
data ValuationPair = ValuationPair Term Term
    deriving (Show, Read, Eq)

-- | Parse 'ValuationPair'
parseValuationPair :: Parser ValuationPair
parseValuationPair = do
  par '('
  t1 <- parseTerm
  t2 <- parseTerm
  par ')'
  return $ ValuationPair t1 t2

-- | Unparse 'ValuationPair'
unparseValuationPair :: ValuationPair -> Text
unparseValuationPair (ValuationPair a b) = 
  T.unwords ["(", unparseTerm a, unparseTerm b, ")"]


-- | @\<t_valuation_pair\> ::= ( \<symbol\> \<b_value\> )@
data TValuationPair = TValuationPair Symbol BValue
    deriving (Show, Read, Eq)

-- | Parse 'TValuationPair'
parseTValuationPair :: Parser TValuationPair
parseTValuationPair = do
  par '('
  symbol <- parseSymbol
  bValue <- parseBValue
  par ')'
  return $ TValuationPair symbol bValue

-- | Unparse 'TValuationPair'
unparseTValuationPair :: TValuationPair -> Text
unparseTValuationPair (TValuationPair symbol bValue) = 
  T.unwords ["(", unparseSymbol symbol, unparseBValue bValue, ")"]


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
  ] <* skipSpace

-- | Unparse 'CheckSatResponse'
unparseCheckSatResponse :: CheckSatResponse -> Text
unparseCheckSatResponse = \case
  Sat     -> "sat"
  Unsat   -> "unsat"
  Unknown -> "unknown"


-- | @\<echo_response\> ::= \<string\>@
newtype EchoResponse = EchoResponse{ unEchoResponse :: String }
  deriving (Show, Read, Eq)

-- | Parse 'EchoResponse'
parseEchoResponse :: Parser EchoResponse
parseEchoResponse = EchoResponse <$> parseString

-- | Unparse 'EchoResponse'
unparseEchoResponse :: EchoResponse -> Text
unparseEchoResponse = unparseString . unEchoResponse


-- | @\<get_assertions_response\> ::= ( \<term\>* )@
newtype GetAssertionsResponse = GetAssertionsResponse [Term]
  deriving (Show, Read, Eq)

-- | Parse 'GetAssertionsResponse'
parseGetAssertionsResponse :: Parser GetAssertionsResponse
parseGetAssertionsResponse = do
  par '('
  terms <- many' parseTerm
  par ')'
  return $ GetAssertionsResponse terms

-- | Unparse 'GetAssertionsResponse'
unparseGetAssertionsResponse :: GetAssertionsResponse -> Text
unparseGetAssertionsResponse (GetAssertionsResponse terms) =
  T.unwords ["(", T.unwords $ unparseTerm <$> terms, ")"]


-- | @\<get_assignment_response\> ::= ( \<t_valuation_pair\>* )@
newtype GetAssignmentResponse = GetAssignmentResponse [TValuationPair]
  deriving (Show, Read, Eq)

-- | Parse 'GetAssignmentResponse'
parseGetAssignmentResponse :: Parser GetAssignmentResponse
parseGetAssignmentResponse = do
  par '('
  tValuationPairs <- many' parseTValuationPair
  par ')'
  return $ GetAssignmentResponse tValuationPairs

-- | Unparse 'GetAssignmentResponse'
unparseGetAssignmentResponse :: GetAssignmentResponse -> Text
unparseGetAssignmentResponse (GetAssignmentResponse tValuationPairs) =
  T.unwords ["(", T.unwords $ unparseTValuationPair <$> tValuationPairs, ")"]


-- | @\<get_info_response\> ::= ( \<info_response\>+ )@
newtype GetInfoResponse = GetInfoResponse (NonEmpty InfoResponse)
  deriving (Show, Read, Eq)

-- | Parse 'GetInfoResponse'
parseGetInfoResponse :: Parser GetInfoResponse
parseGetInfoResponse = do
  par '('
  infoResponses <- NE.fromList <$> many1' parseInfoResponse
  par ')'
  return $ GetInfoResponse infoResponses

-- | Unparse 'GetInfoResponse'
unparseGetInfoResponse :: GetInfoResponse -> Text
unparseGetInfoResponse (GetInfoResponse infoResponses) = 
  T.unwords 
    [ "("
    , T.unwords $ unparseInfoResponse <$> NE.toList infoResponses
    , ")"
    ] 


-- | @\<get_model_response\> ::= ( \<model_response\>* )@
newtype GetModelResponse = GetModelResponse [ModelResponse]
  deriving (Show, Read, Eq)

-- | Parse 'GetModelResponse'
parseGetModelResponse :: Parser GetModelResponse
parseGetModelResponse = do
  par '('
  modelResponses <- many' parseModelResponse
  par ')'
  return $ GetModelResponse modelResponses

-- | Unparse 'GetModelResponse'
unparseGetModelResponse :: GetModelResponse -> Text
unparseGetModelResponse (GetModelResponse modelResponses) =
  T.unwords ["(", T.unwords $ unparseModelResponse <$> modelResponses, ")"]


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
parseGetUnsatAssumptionsResponse = do
  par '('
  symbols <- many' parseSymbol
  par ')'
  return $ GetUnsatAssumptionsResponse symbols

-- | Unparse 'GetUnsatAssumptionsResponse'
unparseGetUnsatAssumptionsResponse :: GetUnsatAssumptionsResponse -> Text
unparseGetUnsatAssumptionsResponse (GetUnsatAssumptionsResponse r) =
  T.unwords ["(", T.unwords $ unparseSymbol <$> r, ")"]


-- | @\<get_unsat_core_response\> ::= ( \<symbol\>* )@
newtype GetUnsatCoreResponse = GetUnsatCoreResponse [Symbol]
  deriving (Show, Read, Eq)

-- | Parse 'GetUnsatCoreResponse'
parseGetUnsatCoreResponse :: Parser GetUnsatCoreResponse
parseGetUnsatCoreResponse = do
  par '('
  symbols <- many' parseSymbol
  par ')'
  return $ GetUnsatCoreResponse symbols

-- | Unparse 'GetUnsatCoreResponse'
unparseGetUnsatCoreResponse :: GetUnsatCoreResponse -> Text
unparseGetUnsatCoreResponse (GetUnsatCoreResponse symbols) =
  T.unwords ["(", T.unwords $ unparseSymbol <$> symbols, ")"]


-- | @\<get_value_response\> ::= ( \<valuation_pair\>+ )@
newtype GetValueResponse = GetValueResponse (NonEmpty ValuationPair)
    deriving (Show, Read, Eq)

-- | Parse 'GetValueResponse'
parseGetValueResponse :: Parser GetValueResponse
parseGetValueResponse = do
  par '('
  valuationPairs <- NE.fromList <$> many1' parseValuationPair
  par ')'
  return $ GetValueResponse valuationPairs

-- | Unparse 'GetValueResponse'
unparseGetValueResponse :: GetValueResponse -> Text
unparseGetValueResponse (GetValueResponse valuationPairs) =
  T.unwords
    [ "("
    , T.unwords $ unparseValuationPair <$> NE.toList valuationPairs
    , ")"
    ]


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
  -- , GeneralResponseSpecificSuccessResponse <$> parseSpecificSuccessResponse
  , GeneralResponseUnsupported <$ string "unsupported"
  -- , parseGeneralResponseError
  ] <* skipSpace
  where
    parseGeneralResponseError = do
      par '('
      "error" *> skipSpace
      str <- parseString
      par ')'
      return $ GeneralResponseError str

-- | Unparse 'GeneralResponse'
unparseGeneralResponse :: GeneralResponse -> Text
unparseGeneralResponse = \case
  GeneralResponseSuccess -> "success"
  GeneralResponseSpecificSuccessResponse specificSuccessResponse ->
    unparseSpecificSuccessResponse specificSuccessResponse
  GeneralResponseUnsupported -> "unsupported"
  GeneralResponseError str ->
    T.unwords ["(", "error", unparseString str, ")"]


syntaxTests :: IO Bool
syntaxTests = checkSequential $$(discover)
