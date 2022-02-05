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

    -- *** SString
    SString (..),
    parseSString,
    unparseSString,

    -- *** SimpleSymbol
    SimpleSymbol(..),
    parseSimpleSymbol,
    unparseSimpleSymbol,

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

import Data.Attoparsec.ByteString.Char8 (
  Parser,
  char,
  choice,
  decimal,
  double,
  hexadecimal,
  isAlpha_iso8859_15,
  isDigit,
  many',
  many1',
  option,
  parseOnly,
  peekChar,
  satisfy,
  skipSpace,
  string,
  takeWhile,
  takeWhile1
  )
import Data.Bits ((.|.), shiftL)
import Data.Char (ord)
import Data.Functor (($>))
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.ByteString.Builder (
  Builder, 
  byteString, 
  char8,
  doubleDec,
  integerDec,
  toLazyByteString,
  wordHex
  )
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as C
import Data.ByteString.Lazy.Char8 (toStrict)
import Prelude hiding (takeWhile)

import           Hedgehog hiding (Command)
import qualified Hedgehog.Gen   as Gen
import qualified Hedgehog.Range as Range


-- | unwords for builders; intersperse spaces.
unwordsB :: [Builder] -> Builder
unwordsB (b:bs@(_:_)) = b <> char8 ' ' <> unwordsB bs
unwordsB [b]          = b
unwordsB []           = mempty 

-- | unlines for builders
unlinesB :: [Builder] -> Builder
unlinesB (b:bs) = b <> char8 '\n' <> unlinesB bs
unlinesB []     = mempty

------------
-- Tokens --
------------

-- | Parse parenthesis
par :: Char -> Parser ()
par p = char p >> skipSpace


-- | Reserved words
newtype Reserved = Reserved{ unReserved :: ByteString }
  deriving (Show, Read, Eq)

reserved :: [ByteString]
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
unparseReserved :: Reserved -> Builder
unparseReserved = byteString . unReserved


-- | @\<numeral\> ::= 0 | a non-empty sequence of digits not starting with 0@
newtype Numeral = Numeral{ unNumeral :: Integer }
  deriving (Show, Read, Eq)

-- | Parse 'Numeral'
parseNumeral :: Parser Numeral
parseNumeral = do
  dec <- decimal
  peekChar >>= \case
    Just '.' -> fail "Numeral cannot contain '.'. Fail to Decimal"
    _ -> Numeral dec <$ skipSpace
    

-- | Unparse 'Numeral'
unparseNumeral :: Numeral -> Builder
unparseNumeral = integerDec . unNumeral

-- | 'parseNumeral' . 'unparseNumeral' == id
prop_numeral_forward :: Property
prop_numeral_forward = property $ do
  n <- forAll $ Gen.integral $ Range.linear 0 (maxBound :: Int)
  let num   = Numeral $ fromIntegral n
      numBs = toStrict $ toLazyByteString $ unparseNumeral num
  parseOnly parseNumeral numBs === Right num


-- | @\<decimal\> ::= \<numeral\>.0*\<numeral\>@
newtype Decimal = Decimal{ unDecimal :: Double }
  deriving (Show, Read, Eq)

-- | Parse 'Decimal'
parseDecimal :: Parser Decimal
parseDecimal = Decimal `fmap` double <* skipSpace

-- | Unparse 'Decimal'
unparseDecimal :: Decimal -> Builder
unparseDecimal = doubleDec . unDecimal


{- |
@
\<hexadecimal\> ::= #x followed by a non-empty sequence of digits and
letters from A to F, capitalized or not
@
-}
newtype Hexadecimal = Hexadecimal{ unHexadecimal :: Integer }
  deriving (Show, Read, Eq)

-- | Parse 'Hexadecimal'
parseHexadecimal :: Parser Hexadecimal
parseHexadecimal = "#x" *> Hexadecimal `fmap` hexadecimal <* skipSpace

-- | Unparse 'Hexadecimal'
unparseHexadecimal :: Hexadecimal -> Builder
unparseHexadecimal (Hexadecimal h) =
  byteString "#x" <> wordHex (fromIntegral h)

-- | 'parseHexadecimal' . 'unparseHexadecimal' == id
prop_hexadecimal_forward :: Property
prop_hexadecimal_forward = property $ do
  n <- forAll $ Gen.integral $ Range.linear 0 (maxBound :: Int)
  let hex   = Hexadecimal $ fromIntegral n
      hexBs = toStrict $ toLazyByteString $ unparseHexadecimal hex
  parseOnly parseHexadecimal hexBs === Right hex


-- | @\<binary\> ::= #b followed by a non-empty sequence of 0 and 1 characters@
newtype Binary = Binary{ unBinary :: Integer }
  deriving (Show, Read, Eq)

-- | Parse 'Binary'
parseBinary :: Parser Binary
parseBinary = "#b" *> Binary `fmap` binary <* skipSpace
  where
    binary = C.foldl' step 0 `fmap` takeWhile1 isBinDigit
      where
        isBinDigit c = c == '0' || c == '1'
        step a c = (a `shiftL` 1) .|. fromIntegral (ord c - 48)

-- | Unparse 'Binary'
unparseBinary :: Binary -> Builder
unparseBinary (Binary bin) = byteString "#b" <> encodeBin bin
  where
    encodeBin :: Integer -> Builder -- messy, i'd prefer a direct encoding
    encodeBin 0 = char8 '0'
    encodeBin p = go p
      where 
        go 0 = mempty
        go n = let (n', r) = n `divMod` 2
                   b = if r == 1 then '1' else '0'
               in go n' <> char8 b

-- | 'parseBinary' . 'unparseBinary' == id
prop_binary_forward :: Property
prop_binary_forward = property $ do
  n <- forAll $ Gen.integral $ Range.linear 0 (maxBound :: Int)
  let bin   = Binary $ fromIntegral n
      binBs = toStrict $ toLazyByteString $ unparseBinary bin
  parseOnly parseBinary binBs === Right bin


{- |
@
\<string\> ::= sequence of whitespace and printable characters in double
quotes with escape sequence ""
@
-}
newtype SString = SString{ unSString :: ByteString }
  deriving (Show, Read, Eq)

-- | Parse 'SString'.
-- See [StackOverflow thread](https://stackoverflow.com/a/35302838/4051020).
parseSString :: Parser SString
parseSString = char '"' *> SString `fmap` escaped <* char '"' <* skipSpace
  where
    normal = takeWhile $ (/= '"')
    escaped = do
      r <- normal
      rs <- many' escaped'
      return $ C.concat $ r:rs
      where
        escaped' = do
          r1 <- normal
          r2 <- quoted
          return $ r1 <> r2
    quoted = do
      _ <- string "\"\""
      res <- normal
      return $ "\"\"" <> res

-- | Unparse 'SString'
unparseSString :: SString -> Builder
unparseSString (SString str) = char8 '"' <> byteString str <> char8 '"'


{- |
@
\<simple_symbol> ::= a non-empty sequence of letters, digits, and the
characters + - / * = % ? ! . $ _ ~ ^ < > \@ that does not start with a digit.
@
-}
newtype SimpleSymbol = SimpleSymbol{ unSimpleSymbol :: ByteString }
  deriving (Show, Read, Eq)

-- | Parse 'SimpleSymbol'
parseSimpleSymbol :: Parser SimpleSymbol
parseSimpleSymbol = do
  c  <- satisfy $ \c -> not (isDigit c) && isSimpleChar c
  cs <- takeWhile isSimpleChar
  skipSpace
  return $ SimpleSymbol $ c `C.cons` cs
  where
    isSimpleChar c = isDigit c || isAlpha_iso8859_15 c || isSpecial c
    isSpecial c =    c == '+'
                  || c == '-'
                  || c == '/'
                  || c == '*'
                  || c == '='
                  || c == '%'
                  || c == '?'
                  || c == '!'
                  || c == '.'
                  || c == '$'
                  || c == '_'
                  || c == '~'
                  || c == '^'
                  || c == '<'
                  || c == '>'
                  || c == '@'
                  
-- | Unparse 'SimpleSymbol'
unparseSimpleSymbol :: SimpleSymbol -> Builder
unparseSimpleSymbol = byteString . unSimpleSymbol


{- |
@
\<symbol\> ::= \<simple_symbol\>
           | a sequence of whitespace and printable characters that starts
             and ends with | and does not otherwise include | or \
@
-}
data Symbol = SymbolSimpleSymbol SimpleSymbol
            | SymbolQuoted ByteString
  deriving (Show, Read, Eq)

-- | Parse 'Symbol'
parseSymbol :: Parser Symbol
parseSymbol = choice
  [ SymbolSimpleSymbol <$> parseSimpleSymbol 
  , -- what about '/'?
    char '|' *> SymbolQuoted `fmap` takeWhile (/= '|') <* char '|' <* skipSpace
  ]

-- | Unparse 'Symbol'
unparseSymbol :: Symbol -> Builder
unparseSymbol = \case
  SymbolSimpleSymbol simpleSymbol -> unparseSimpleSymbol simpleSymbol
  SymbolQuoted quote -> char8 '|' <> byteString quote <> char8 '|'


-- | @\<keyword\> ::= :\<simple_symbol\>@
newtype Keyword = Keyword{ unKeyword :: SimpleSymbol }
  deriving (Show, Read, Eq)

-- | Parse 'Keyword'
parseKeyword :: Parser Keyword
parseKeyword = char ':' *> Keyword `fmap` parseSimpleSymbol

-- | Unparse 'Keyword'
unparseKeyword :: Keyword -> Builder
unparseKeyword (Keyword simpleSymbol) = 
  char8 ':' <> unparseSimpleSymbol simpleSymbol


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
    SpecConstantString SString
  deriving (Show, Read, Eq)

-- | Parse 'SpecConstant'
parseSpecConstant :: Parser SpecConstant
parseSpecConstant = choice
  [ SpecConstantNumeral     <$> parseNumeral
  , SpecConstantDecimal     <$> parseDecimal -- order matters
  , SpecConstantHexadecimal <$> parseHexadecimal
  , SpecConstantBinary      <$> parseBinary
  , SpecConstantString      <$> parseSString
  ]

-- | Unparse 'SpecConstant'
unparseSpecConstant :: SpecConstant -> Builder
unparseSpecConstant = \case
  SpecConstantNumeral     num -> unparseNumeral     num
  SpecConstantDecimal     dec -> unparseDecimal     dec
  SpecConstantHexadecimal hex -> unparseHexadecimal hex
  SpecConstantBinary      bin -> unparseBinary      bin
  SpecConstantString      str -> unparseSString     str


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
  , par '(' *> SExprs `fmap` many' parseSExpr <* par ')'
  ]

-- | Unparse 'SExpr'
unparseSExpr :: SExpr -> Builder
unparseSExpr = \case
  SExprSpecConstant specConstant -> unparseSpecConstant specConstant
  SExprSymbol       symbol       -> unparseSymbol symbol
  SExprReserved     rsvd         -> unparseReserved rsvd
  SExprKeyword      keyword      -> unparseKeyword keyword
  SExprs            exprs        -> 
    unwordsB [char8 '(', unwordsB $ unparseSExpr <$> exprs, char8 ')']


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
unparseIndex :: Index -> Builder
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
unparseIdentifier :: Identifier -> Builder
unparseIdentifier = \case
  IdentifierSymbol sym -> unparseSymbol sym
  IdentifierUnderscore symbol indices ->
    unwordsB [ char8 '('
             , char8 '_'
             , unparseSymbol symbol
             , unwordsB $ unparseIndex <$> NE.toList indices
             , char8 ')'
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
unparseSort :: Sort -> Builder
unparseSort = \case
  Sort identifier []    -> unparseIdentifier identifier
  Sort identifier sorts -> 
    unwordsB [ char8 '('
             , unparseIdentifier identifier
             , unwordsB $ unparseSort <$> sorts
             , char8 ')'
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
  , par '(' *> AttributeValueSExprs `fmap` many' parseSExpr <* par ')'
  ]

-- | Unparse 'AttributeValue'
unparseAttributeValue :: AttributeValue -> Builder
unparseAttributeValue = \case
  AttributeValueSpecConstant specConstant -> unparseSpecConstant specConstant
  AttributeValueSymbol       symbol       -> unparseSymbol symbol
  AttributeValueSExprs       exprs        ->
    unwordsB [char8 '(', unwordsB $ unparseSExpr <$> exprs, char8 ')']


-- | @\<attribute\> ::= \<keyword\> | \<keyword\> \<attribute_value\>@
data Attribute
  = -- | \<keyword\>
    AttributeKeyword Keyword
  | -- | \<keyword\> \<attribute_value\>
    AttributeKeywordAttributeValue Keyword AttributeValue
  deriving (Show, Read, Eq)

-- | Parse 'Attribute'
parseAttribute :: Parser Attribute
parseAttribute = do
  keyword <- parseKeyword
  option
    (AttributeKeyword keyword)
    (AttributeKeywordAttributeValue keyword <$> parseAttributeValue)

-- | Unparse 'Attribute'
unparseAttribute :: Attribute -> Builder
unparseAttribute = \case
  AttributeKeyword keyword -> unparseKeyword keyword
  AttributeKeywordAttributeValue keyword attributeValue ->
    unwordsB [unparseKeyword keyword, unparseAttributeValue attributeValue]


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
unparseQualIdentifier :: QualIdentifier -> Builder
unparseQualIdentifier = \case
    QualIdentifier   identifier      -> unparseIdentifier identifier
    QualIdentifierAs identifier sort -> 
      unwordsB [ char8 '('
               , byteString "as"
               , unparseIdentifier identifier
               , unparseSort sort
               , char8 ')'
               ]


-- | @\<var_binding\> ::= ( \<symbol\> \<term\> )@
data VarBinding = VarBinding
  { varBindingSymbol :: Symbol
  , varBindingTerm   :: Term
  }
  deriving (Show, Read, Eq)

-- | Parse 'VarBinding'
parseVarBinding :: Parser VarBinding
parseVarBinding = do
  par '('
  symbol <- parseSymbol
  term   <- parseTerm
  par ')'
  return $ VarBinding{ varBindingSymbol = symbol
                     , varBindingTerm   = term
                     }

-- | Unparse 'VarBinding'
unparseVarBinding :: VarBinding -> Builder
unparseVarBinding (VarBinding symbol term) =
    unwordsB [char8 '(', unparseSymbol symbol, unparseTerm term, char8 ')']


-- | @\<sorted_var\> ::= ( \<symbol\> \<sort\> )@
data SortedVar = SortedVar
  { sortedVarSymbol :: Symbol
  , sortedVarSort   :: Sort
  }
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
unparseSortedVar :: SortedVar -> Builder
unparseSortedVar (SortedVar symbol sort) = 
  unwordsB [char8 '(', unparseSymbol symbol, unparseSort sort, char8 ')']


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
unparsePattern :: Pattern -> Builder
unparsePattern = \case
  Pattern  symbol         -> unparseSymbol symbol
  Patterns symbol symbols -> 
    unwordsB [ char8 '('
             , unparseSymbol symbol
             , unwordsB $ unparseSymbol <$> NE.toList symbols
             , char8 ')'
             ]


-- | @\<match_case\> ::= ( \<pattern\> \<term\> )@
data MatchCase = MatchCase
  { matchCasePattern :: Pattern
  , matchCaseTerm    :: Term
  }
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
unparseMatchCase :: MatchCase -> Builder
unparseMatchCase matchCase =
  unwordsB 
    [ char8 '('
    , unparsePattern $ matchCasePattern matchCase
    , unparseTerm    $ matchCaseTerm    matchCase
    , char8 ')'
    ]


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
unparseTerm :: Term -> Builder
unparseTerm = \case
  TermSpecConstant specConstant -> unparseSpecConstant specConstant
  TermQualIdentifier qualIdentifier -> unparseQualIdentifier qualIdentifier
  TermQualIdentifiers qualIdentifier terms ->
    unwordsB
      [ char8 '('
      , unparseQualIdentifier qualIdentifier
      , unwordsB $ unparseTerm <$> NE.toList terms
      , char8 ')'
      ]
  TermLet varBindings term -> 
    unwordsB 
      [ char8 '('
      , byteString "let"
      , char8 '('
      , unwordsB $ unparseVarBinding <$> NE.toList varBindings
      , char8 ')'
      , unparseTerm term
      , char8 ')'
      ]
  TermForall sortedVars term -> 
    unwordsB 
      [ char8 '('
      , byteString "forall"
      , char8 '('
      , unwordsB $ unparseSortedVar <$> NE.toList sortedVars
      , char8 ')'
      , unparseTerm term
      , char8 ')'
      ]
  TermExists sortedVars term ->
    unwordsB
      [ char8 '('
      , byteString "exists"
      , char8 '('
      , unwordsB $ unparseSortedVar <$> NE.toList sortedVars
      , char8 ')'
      , unparseTerm term
      , char8 ')'
      ]
  TermMatch term matchCases ->
    unwordsB
      [ char8 '('
      , byteString "match"
      , unparseTerm term
      , char8 '('
      , unwordsB $ unparseMatchCase <$> NE.toList matchCases
      , char8 ')'
      , char8 ')'
      ]
  TermExclamation term attributes ->
    unwordsB
      [ char8 '('
      , char8 '!'
      , unparseTerm term
      , unwordsB $ unparseAttribute <$> NE.toList attributes
      , char8 ')'
      ]


--------------
-- Theories --
--------------

-- | @\<sort_symbol_decl\> ::= ( \<identifier\> \<numeral\> \<attribute\>* )@
data SortSymbolDecl = SortSymbolDecl
  { sortSymbolDeclIdentifier :: Identifier 
  , sortSymbolDeclNumeral    :: Numeral 
  , sortSymbolDeclAttributes :: [Attribute]
  }
  deriving (Show, Read, Eq)

-- | Parse 'SortSymbolDecl'
parseSortSymbolDecl :: Parser SortSymbolDecl
parseSortSymbolDecl = do
  par '('
  identifier <- parseIdentifier
  numeral    <- parseNumeral
  attributes <- many' parseAttribute
  par ')'
  return $ SortSymbolDecl{ sortSymbolDeclIdentifier = identifier
                         , sortSymbolDeclNumeral    = numeral 
                         , sortSymbolDeclAttributes = attributes
                         }

-- | Unparse 'SortSymbolDecl'
unparseSortSymbolDecl :: SortSymbolDecl -> Builder
unparseSortSymbolDecl sortSymbolDecl =
  unwordsB 
    [ char8 '('
    ,            unparseIdentifier $  sortSymbolDeclIdentifier sortSymbolDecl
    ,            unparseNumeral    $  sortSymbolDeclNumeral    sortSymbolDecl
    , unwordsB $ unparseAttribute <$> sortSymbolDeclAttributes sortSymbolDecl
    , char8 ')'
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
unparseMetaSpecConstant :: MetaSpecConstant -> Builder
unparseMetaSpecConstant = byteString . \case
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
unparseFunSymbolDecl :: FunSymbolDecl -> Builder
unparseFunSymbolDecl = \case
  FunSymbolDeclSpecConstant specConstant sort attributes ->
    unwordsB
      [ char8 '('
      , unparseSpecConstant specConstant
      , unparseSort sort
      , unwordsB $ unparseAttribute <$> attributes
      , char8 ')'
      ]
  FunSymbolDeclMetaSpecConstant metaSpecConstant sort attributes ->
    unwordsB
      [ char8 '('
      , unparseMetaSpecConstant metaSpecConstant
      , unparseSort sort
      , unwordsB $ unparseAttribute <$> attributes
      , char8 ')'
      ]
  FunSymbolDeclIdentifier identifier sorts attributes ->
    unwordsB
      [ char8 '('
      , unparseIdentifier identifier
      , unwordsB $ unparseSort <$> NE.toList sorts
      , unwordsB $ unparseAttribute <$> attributes
      , char8 ')'
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
unparseParFunSymbolDecl :: ParFunSymbolDecl -> Builder
unparseParFunSymbolDecl = \case
  ParFunSymbolDeclFunSymbolDecl funSymbolDecl ->
    unparseFunSymbolDecl funSymbolDecl
  Par symbols identifier sorts attributes ->
    unwordsB
      [ char8 '('
      , byteString "par"
      , char8 '('
      , unwordsB $ unparseSymbol <$> NE.toList symbols
      , char8 ')'
      , char8 '('
      , unparseIdentifier identifier
      , unwordsB $ unparseSort <$> NE.toList sorts
      , unwordsB $ unparseAttribute <$> attributes
      , char8 ')'
      , char8 ')'
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
    TheoryAttributeSortsDescription SString

  | -- | :funs-description \<string\>
    TheoryAttributeFunsDescription SString

  | -- | :defintion \<string\>
    TheoryAttributeDefinition SString

  | -- | :values \<string\>
    TheoryAttributeValues SString

  | -- | :notes \<string\>
    TheoryAttributeNotes SString

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
  , ":definition" *> skipSpace >> TheoryAttributeDefinition <$> parseSString
  , ":values"     *> skipSpace >> TheoryAttributeValues     <$> parseSString
  , ":notes"      *> skipSpace >> TheoryAttributeNotes      <$> parseSString
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
      TheoryAttributeSortsDescription <$> parseSString
    parseTheoryAttributeFunsDescription = do
      ":funs-description" *> skipSpace
      TheoryAttributeFunsDescription <$> parseSString

-- | Unparse 'TheoryAttribute'
unparseTheoryAttribute :: TheoryAttribute -> Builder
unparseTheoryAttribute = \case
  TheoryAttributeSorts sortSymbolDecls ->
    unwordsB
      [ byteString ":sorts"
      , char8 '('
      , unwordsB $ unparseSortSymbolDecl <$> NE.toList sortSymbolDecls
      , char8 ')'
      ]
  TheoryAttributeFuns parFunSymbolDecls ->
    unwordsB
      [ byteString ":funs"
      , char8 '('
      , unwordsB $ unparseParFunSymbolDecl <$> NE.toList parFunSymbolDecls
      , char8 ')'
      ]
  TheoryAttributeSortsDescription str ->
    unwordsB [byteString ":sorts-description", unparseSString str]
  TheoryAttributeFunsDescription str ->
    unwordsB [byteString ":funs-description", unparseSString str]
  TheoryAttributeDefinition str ->
    unwordsB [byteString ":definition", unparseSString str]
  TheoryAttributeValues str ->
    unwordsB [byteString ":values", unparseSString str]
  TheoryAttributeNotes str ->
    unwordsB [byteString ":notes", unparseSString str]
  TheoryAttributeAttribute attribute -> unparseAttribute attribute


-- | @\<theory_decl\> ::= ( theory \<symbol\> \<theory_attribute\>+ )@
data TheoryDecl = TheoryDecl 
  { theoryDeclSymbol           :: Symbol 
  , theoryDeclTheoryAttributes :: NonEmpty TheoryAttribute
  }
  deriving (Show, Read, Eq)

-- | Parse 'TheoryDecl'
parseTheoryDecl :: Parser TheoryDecl
parseTheoryDecl = do
  par '('
  "theory" *> skipSpace
  symbol           <- parseSymbol
  theoryAttributes <- NE.fromList <$> many1' parseTheoryAttribute
  par ')'
  return $ TheoryDecl{ theoryDeclSymbol           = symbol
                     , theoryDeclTheoryAttributes = theoryAttributes
                     }

-- | Unparse 'TheoryDecl'
unparseTheoryDecl :: TheoryDecl -> Builder
unparseTheoryDecl (TheoryDecl symbol theoryAttributes) = unwordsB
  [ char8 '('
  , byteString "theory"
  , unparseSymbol symbol
  , unwordsB $ unparseTheoryAttribute <$> NE.toList theoryAttributes
  , char8 ')'
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
    LogicAttributeLanguage SString

  | -- | :extensions \<string\>
    LogicAttributeExtensions SString

  | -- | :values \<string\>
    LogicAttributeValues SString

  | -- | :notes \<string\>
    LogicAttributeNotes SString

  | -- | \<attribute\>
    LogicAttributeAttribute Attribute
  deriving (Show, Read, Eq)

-- | Parse 'LogicAttribute'
parseLogicAttribute :: Parser LogicAttribute
parseLogicAttribute = choice
  [ parseLogicAttributeTheories
  , ":language"   *> skipSpace >> LogicAttributeLanguage   <$> parseSString
  , ":extensions" *> skipSpace >> LogicAttributeExtensions <$> parseSString
  , ":values"     *> skipSpace >> LogicAttributeValues     <$> parseSString
  , ":notes"      *> skipSpace >> LogicAttributeNotes      <$> parseSString
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
unparseLogicAttribute :: LogicAttribute -> Builder
unparseLogicAttribute = \case
  LogicAttributeTheories symbols ->
    unwordsB
      [ byteString ":theories"
      , char8 '('
      , unwordsB $ unparseSymbol <$> NE.toList symbols
      , char8 ')'
      ]
  LogicAttributeLanguage   str ->
    unwordsB [byteString ":language", unparseSString str]
  LogicAttributeExtensions str ->
    unwordsB [byteString ":extensions", unparseSString str]
  LogicAttributeValues     str ->
    unwordsB [byteString ":values", unparseSString str]
  LogicAttributeNotes      str ->
    unwordsB [byteString ":notes", unparseSString str]
  LogicAttributeAttribute attribute -> unparseAttribute attribute


-- | @\<logic\> ::= ( logic \<symbol\> \<logic_attribute\>+ )@
data Logic = Logic
  { logicSymbol          :: Symbol
  , logicLogicAttribtues :: NonEmpty LogicAttribute
  }
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
unparseLogic :: Logic -> Builder
unparseLogic (Logic symbol logicAttributes) = unwordsB
  [ char8 '('
  , byteString "logic"
  , unparseSymbol symbol
  , unwordsB $ unparseLogicAttribute <$> NE.toList logicAttributes
  , char8 ')'
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
unparseInfoFlag :: InfoFlag -> Builder
unparseInfoFlag = \case
  InfoFlagAllStatistics        -> byteString ":all-statistics"
  InfoFlagAssertionStackLevels -> byteString ":assertion-stack-levels"
  InfoFlagAuthors              -> byteString ":authors"
  InfoFlagErrorBehavior        -> byteString ":error-behavior"
  InfoFlagName                 -> byteString ":name"
  InfoFlagReasonUnknown        -> byteString ":reason-unknown"
  InfoFlagVersion              -> byteString ":version"
  InfoFlagKeyword keyword      -> unparseKeyword keyword

---------------------
-- Command options --
---------------------

-- | @\<b_value\> ::= true | false@
newtype BValue = BValue{ unBValue :: Bool }
  deriving (Show, Read, Eq)

-- | Parse 'BValue'
parseBValue :: Parser BValue
parseBValue = choice 
  [ BValue True  <$ string "true"
  , BValue False <$ string "false"
  ] <* skipSpace

-- | Unparse 'BValue'
unparseBValue :: BValue -> Builder
unparseBValue (BValue True ) = byteString "true"
unparseBValue (BValue False) = byteString "false"


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
    OptionDiagnosticOutputChannel SString

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
    OptionRegularOutputChannel SString

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
      OptionDiagnosticOutputChannel   `fmap` parseSString
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
      OptionRegularOutputChannel      `fmap` parseSString
  , ":reproducible-resource-limit"     *>
      OptionReproducibleResourceLimit `fmap` parseNumeral
  , ":verbosity"                       *>
      OptionVerbosity                  `fmap` parseNumeral
  , OptionAttribute                   <$> parseAttribute
  ]

-- | Unparse 'Option'
unparseOption :: Option -> Builder
unparseOption = \case
  OptionDiagnosticOutputChannel   str ->
    unwordsB [byteString ":diagnostic-output-channel"  , unparseSString  str]
  OptionGlobalDeclarations        b   ->
    unwordsB [byteString ":global-declarations"        , unparseBValue  b   ]
  OptionInteractiveMode           b   ->
    unwordsB [byteString ":interactive-mode"           , unparseBValue  b   ]
  OptionPrintSuccess              b   ->
    unwordsB [byteString ":print-success"              , unparseBValue  b   ]
  OptionProduceAssertions         b   ->
    unwordsB [byteString ":produce-assertions"         , unparseBValue  b   ]
  OptionProduceAssignments        b   ->
    unwordsB [byteString ":produce-assignments"        , unparseBValue  b   ]
  OptionProduceModels             b   ->
    unwordsB [byteString ":produce-models"             , unparseBValue  b   ]
  OptionProduceProofs             b   ->
    unwordsB [byteString ":produce-proofs"             , unparseBValue  b   ]
  OptionProduceUnsatAssumptions   b   ->
    unwordsB [byteString ":produce-unsat-assumptions"  , unparseBValue  b   ]
  OptionProduceUnsatCores         b   ->
    unwordsB [byteString ":produce-unsat-cores"        , unparseBValue  b   ]
  OptionRandomSeed                n   ->
    unwordsB [byteString ":random-seed"                , unparseNumeral n   ]
  OptionRegularOutputChannel      str ->
    unwordsB [byteString ":regular-output-channel"     , unparseSString  str]
  OptionReproducibleResourceLimit n   ->
    unwordsB [byteString ":reproducible-resource-limit", unparseNumeral n   ]
  OptionVerbosity                 n   ->
    unwordsB [byteString ":verbosity"                  , unparseNumeral n   ]
  OptionAttribute attribute -> unparseAttribute attribute

--------------
-- Commands --
--------------

-- | @\<sort_dec\> ::= ( \<symbol\> \<numeral\> )@
data SortDec = SortDec
  { sortDecSymbol  :: Symbol
  , sortDecNumeral :: Numeral
  }
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
unparseSortDec :: SortDec -> Builder
unparseSortDec sortDec = unwordsB 
  [ char8 '('
  , unparseSymbol  $ sortDecSymbol  sortDec
  , unparseNumeral $ sortDecNumeral sortDec
  , char8 ')'
  ]


-- | @\<selector_dec\> ::= ( \<symbol\> \<sort\> )@
data SelectorDec = SelectorDec 
  { selectorDecSymbol :: Symbol
  , selectorDecSort   :: Sort
  }
  deriving (Show, Read, Eq)

-- | Parse 'SelectorDec'
parseSelectorDec :: Parser SelectorDec
parseSelectorDec = do
  par '('
  symbol <- parseSymbol
  sort   <- parseSort
  par ')'
  return $ SelectorDec{ selectorDecSymbol = symbol 
                      , selectorDecSort   = sort
                      }

-- | Unparse 'SelectorDec'
unparseSelectorDec :: SelectorDec -> Builder
unparseSelectorDec selectorDec = unwordsB
  [ char8 '('
  , unparseSymbol $ selectorDecSymbol selectorDec
  , unparseSort   $ selectorDecSort   selectorDec
  , char8 ')'
  ]


-- | @\<constructor_dec\> ::= ( \<symbol\> \<selector_dec\>* )@
data ConstructorDec = ConstructorDec
  { constructorDecSymbol       :: Symbol
  , constructorDecSelectorDecs :: [SelectorDec]
  }
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
unparseConstructorDec :: ConstructorDec -> Builder
unparseConstructorDec (ConstructorDec symbol selectorDecs) = unwordsB
  [ char8 '('
  , unparseSymbol symbol
  , unwordsB $ unparseSelectorDec <$> selectorDecs
  , char8 ')'
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
unparseDatatypeDec :: DatatypeDec -> Builder
unparseDatatypeDec = \case
  DatatypeDec constructorDecs ->
    unwordsB 
      [ char8 '('
      , unwordsB $ unparseConstructorDec <$> NE.toList constructorDecs
      , char8 ')'
      ]
  DatatypeDecPar symbols constructorDecs ->
    unwordsB
      [ char8 '('
      , byteString "par"
      , char8 '('
      , unwordsB $ unparseSymbol <$> NE.toList symbols
      , char8 ')'
      , char8 '('
      , unwordsB $ unparseConstructorDec <$> NE.toList constructorDecs
      , char8 ')'
      , char8 ')'
      ]


-- | @\<function_dec\> ::= ( \<symbol\> ( \<sorted_var\>* ) \<sort\> )@
data FunctionDec = FunctionDec
  { functionDecSymbol     :: Symbol
  , functionDecSortedVars :: [SortedVar]
  , functionDecSort       :: Sort
  }
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
unparseFunctionDec :: FunctionDec -> Builder
unparseFunctionDec (FunctionDec symbol sortedVars sort) = unwordsB
  [ char8 '('
  , unparseSymbol symbol
  , char8 '('
  , unwordsB $ unparseSortedVar <$> sortedVars
  , char8 ')'
  , unparseSort sort
  , char8 ')'
  ]


-- | @\<function_def\> ::= \<symbol\> ( \<sorted_var\>* ) \<sort\> \<term\>@
data FunctionDef = FunctionDef
  { functionDefSymbol     :: Symbol
  , functionDefSortedVars :: [SortedVar]
  , functionDefSort       :: Sort
  , functionDefTerm       :: Term
  }
  deriving (Show, Read, Eq)

-- | Parse 'FunctionDef'
parseFunctionDef :: Parser FunctionDef
parseFunctionDef = do
  symbol     <- parseSymbol
  par '('
  sortedVars <- many' parseSortedVar
  par ')'
  FunctionDef symbol sortedVars <$> parseSort <*> parseTerm

unparseFunctionDef :: FunctionDef -> Builder
unparseFunctionDef (FunctionDef symbol sortedVars sort term) = unwordsB
  [ unparseSymbol symbol
  , char8 '('
  , unwordsB $ unparseSortedVar <$> sortedVars
  , char8 ')'
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
unparsePropLiteral :: PropLiteral -> Builder
unparsePropLiteral = \case
  PropLiteralSymbol symbol -> unparseSymbol symbol
  PropLiteralNotSymbol symbol ->
    unwordsB 
      [ char8 '('
      , byteString "not"
      , unparseSymbol symbol
      , char8 ')'
      ]


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
    Echo SString
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
    parseCheckSat = par '(' *> "check-sat" *> skipSpace <* par ')' $> CheckSat
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
      str <- parseSString
      par ')'
      return $ Echo str
    parseExit = par '(' *> "exit" *> skipSpace <* par ')' $> Exit
    parseGetAssertions =
      par '(' *> "get-assertions" *> skipSpace <* par ')' $> GetAssertions
    parseGetAssignment = do
      par '(' *> "get-assignment" *> skipSpace <* par ')' $> GetAssignment
    parseGetInfo = do
      par '('
      "get-info" *> skipSpace
      infoFlag <- parseInfoFlag
      par ')'
      return $ GetInfo infoFlag
    parseGetModel =
      par '(' *> "get-model" *> skipSpace <* par ')' $> GetModel
    parseGetOption = do
      par '('
      "get-option" *> skipSpace
      keyword <- parseKeyword
      par ')'
      return $ GetOption keyword
    parseGetProof =
      par '(' *> "get-proof" *> skipSpace <* par ')' $> GetProof
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
    parseReset = par '(' *> "reset" *> skipSpace <* par ')' $> Reset
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
      opt <- parseOption
      par ')'
      return $ SetOption opt

-- | Unparse 'Command'
unparseCommand :: Command -> Builder
unparseCommand = \case
  Assert term ->
    unwordsB 
      [ char8 '('
      , byteString "assert"
      , unparseTerm term
      , char8 ')'
      ]
  CheckSat -> byteString "( check-sat )"
  CheckSatAssuming propLiterals ->
    unwordsB
      [ char8 '('
      , byteString "check-sat-assuming"
      , char8 '('
      , unwordsB $ unparsePropLiteral <$> propLiterals
      , char8 ')'
      , char8 ')'
      ]
  DeclareConst symbol sort ->
    unwordsB
      [ char8 '('
      , byteString "declare-const"
      , unparseSymbol symbol
      , unparseSort sort
      , char8 ')'
      ]
  DeclareDatatype symbol datatypeDec ->
    unwordsB
      [ char8 '('
      , byteString "declare-datatype"
      , unparseSymbol symbol
      , unparseDatatypeDec datatypeDec
      , char8 ')'
      ]
  DeclareDatatypes sortDecsDatatypeDecs ->
    unwordsB
      [ char8 '('
      , byteString "declare-datatypes"
      , char8 '('
      , unwordsB $ unparseSortDec     . fst <$> NE.toList sortDecsDatatypeDecs
      , char8 ')'
      , char8 '('
      , unwordsB $ unparseDatatypeDec . snd <$> NE.toList sortDecsDatatypeDecs
      , char8 ')'
      , char8 ')'
      ]
  DeclareFun symbol sorts sort ->
    unwordsB
      [ char8 '('
      , byteString "declare-fun"
      , unparseSymbol symbol
      , char8 '('
      , unwordsB $ unparseSort <$> sorts
      , char8 ')'
      , unparseSort sort
      , char8 ')'
      ]
  DeclareSort symbol numeral ->
    unwordsB
      [ char8 '('
      , byteString "declare-sort"
      , unparseSymbol symbol
      , unparseNumeral numeral
      , char8 ')'
      ]
  DefineFun functionDef -> 
    unwordsB
      [ char8 '('
      , byteString "define-fun"
      , unparseFunctionDef functionDef
      , char8 ')'
      ]
  DefineFunRec functionDef ->
    unwordsB
      [ char8 '('
      , byteString "define-fun-rec"
      , unparseFunctionDef functionDef
      , char8 ')'
      ]
  DefineFunsRec functionDecsTerms ->
    unwordsB
      [ char8 '('
      , byteString "define-funs-rec"
      , char8 '('
      , unwordsB $ unparseFunctionDec . fst <$> NE.toList functionDecsTerms
      , char8 ')'
      , char8 '('
      , unwordsB $ unparseTerm . snd <$> NE.toList functionDecsTerms
      , char8 ')'
      , char8 ')'
      ]
  DefineSort symbol symbols sort ->
    unwordsB
      [ char8 '('
      , byteString "define-sort"
      , unparseSymbol symbol
      , char8 '('
      , unwordsB $ unparseSymbol <$> symbols
      , char8 ')'
      , unparseSort sort
      , char8 ')'
      ]
  Echo str ->
    unwordsB 
      [ char8 '('
      , byteString "echo"
      , unparseSString str
      , char8 ')'
      ]
  Exit -> byteString "( exit )"
  GetAssertions -> byteString "( get-assertions )"
  GetAssignment -> byteString "( get-assignment )"
  GetInfo infoFlag ->
    unwordsB
      [ char8 '('
      , byteString "get-info"
      , unparseInfoFlag infoFlag
      , char8 ')'
      ]
  GetModel -> byteString "( get-model )"
  GetOption keyword ->
    unwordsB
      [ char8 '('
      , byteString "get-option"
      , unparseKeyword keyword
      , char8 ')'
      ]
  GetProof -> byteString "( get-proof )"
  GetUnsatAssumptions -> byteString "( get-unsat-assumptions )"
  GetUnsatCore -> byteString "( get-unsat-core )"
  GetValue terms ->
    unwordsB
      [ char8 '('
      , byteString "get-value"
      , char8 '('
      , unwordsB $ unparseTerm <$> NE.toList terms
      , char8 ')'
      , char8 ')'
      ]
  Pop num ->
    unwordsB
      [ char8 '('
      , byteString "pop"
      ,  unparseNumeral num
      , char8 ')'
      ]
  Push num ->
    unwordsB
      [ char8 '('
      , byteString "push"
      , unparseNumeral num
      , char8 ')'
      ]
  Reset -> byteString "( reset )"
  ResetAssertions -> byteString "( reset-assertions )"
  SetInfo attribute ->
    unwordsB
      [ char8 '('
      , byteString "set-info"
      , unparseAttribute attribute
      , char8 ')'
      ]
  SetLogic symbol ->
    unwordsB
      [ char8 '('
      , byteString "set-logic"
      , unparseSymbol symbol
      , char8 ')'
      ]
  SetOption opt ->
    unwordsB
      [ char8 '('
      , byteString "set-option"
      , unparseOption opt
      , char8 ')'
      ]


-- | @\<script\> ::= \<command\>*@
newtype Script = Script{ unScript :: [Command] }
  deriving (Show, Read, Eq)

-- | Parse 'Script'
parseScript :: Parser Script
parseScript = Script <$> many' parseCommand

-- | Unparse 'Script'
unparseScript :: Script -> Builder
unparseScript (Script commands) = unlinesB $ unparseCommand <$> commands


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
unparseErrorBehavior :: ErrorBehavior -> Builder
unparseErrorBehavior = \case
  ImmediateExit      -> byteString "immediate-exit"
  ContinuedExecution -> byteString "continued-execution"


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
unparseReasonUnknown :: ReasonUnknown -> Builder
unparseReasonUnknown = \case
  Memout              -> byteString "memout"
  Incomplete          -> byteString "incomplete"
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
unparseModelResponse :: ModelResponse -> Builder
unparseModelResponse = \case
  ModelResponseDefineFun functionDef ->
    unwordsB
      [ char8 '('
      , byteString "define-fun"
      , unparseFunctionDef functionDef
      , char8 ')'
      ]
  ModelResponseDefineFunRec functionDef ->
    unwordsB
      [ char8 '('
      , byteString "define-fun-rec"
      , unparseFunctionDef functionDef
      , char8 ')'
      ]
  ModelResponseDefineFunsRec functionDecsTerms ->
    unwordsB
      [ char8 '('
      , byteString "define-funs-rec"
      , char8 '('
      , unwordsB $ unparseFunctionDec . fst <$> NE.toList functionDecsTerms
      , char8 ')'
      , char8 '('
      , unwordsB $ unparseTerm . snd <$> NE.toList functionDecsTerms
      , char8 ')'
      , char8 ')'
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
    InfoResponseAuthors SString
  | -- | :error-behavior \<error-behavior\>
    InfoResponseErrorBehavior ErrorBehavior
  | -- | :name \<string\>
    InfoResponseName SString
  | -- | :reason-unknown \<reason-unknown\>
    InfoResponseReasonUnknown ReasonUnknown
  | -- | :version \<string\>
    InfoResponseVersion SString
  | -- | \<attribute\>
    InfoResponseAttribute Attribute
  deriving (Show, Read, Eq)

-- | Parse 'InfoResponse'
parseInfoResponse :: Parser InfoResponse
parseInfoResponse = choice
  [ parseInfoResponseAssertionStackLevels
  , ":authors" *> skipSpace >> InfoResponseAuthors <$> parseSString
  , parseInfoResponseErrorBehavior
  , ":name" *> skipSpace >> InfoResponseName <$> parseSString
  , parseInfoResponseReasonUnknown
  , ":version" *> skipSpace >> InfoResponseVersion <$> parseSString
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
unparseInfoResponse :: InfoResponse -> Builder
unparseInfoResponse = \case
  InfoResponseAssertionStackLevels num ->
    unwordsB [byteString ":assertion-stack-levels", unparseNumeral num]
  InfoResponseAuthors str ->
    unwordsB [byteString ":authors", unparseSString str]
  InfoResponseErrorBehavior errorBehavior ->
    unwordsB [byteString":error-behavior", unparseErrorBehavior errorBehavior]
  InfoResponseName str ->
    unwordsB [byteString ":name", unparseSString str]
  InfoResponseReasonUnknown reasonUnknown ->
    unwordsB [byteString ":reason-unknown", unparseReasonUnknown reasonUnknown]
  InfoResponseVersion str -> 
    unwordsB [byteString ":version", unparseSString str]
  InfoResponseAttribute attribute ->
    unparseAttribute attribute


-- | @\<valuation_pair\> ::= ( \<term\> \<term\> )@
data ValuationPair = ValuationPair
  { valuationPairFst :: Term
  , valuationPairSnd :: Term
  }
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
unparseValuationPair :: ValuationPair -> Builder
unparseValuationPair (ValuationPair a b) = 
  unwordsB [char8 '(', unparseTerm a, unparseTerm b, char8 ')']


-- | @\<t_valuation_pair\> ::= ( \<symbol\> \<b_value\> )@
data TValuationPair = TValuationPair
  { tValuationPairSymbol :: Symbol
  , tValuationPairBValue :: BValue
  }
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
unparseTValuationPair :: TValuationPair -> Builder
unparseTValuationPair (TValuationPair symbol bValue) = unwordsB
  [ char8 '('
  , unparseSymbol symbol
  , unparseBValue bValue
  , char8 ')'
  ]


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
unparseCheckSatResponse :: CheckSatResponse -> Builder
unparseCheckSatResponse = byteString . \case
  Sat     -> "sat"
  Unsat   -> "unsat"
  Unknown -> "unknown"


-- | @\<echo_response\> ::= \<string\>@
newtype EchoResponse = EchoResponse{ unEchoResponse :: SString }
  deriving (Show, Read, Eq)

-- | Parse 'EchoResponse'
parseEchoResponse :: Parser EchoResponse
parseEchoResponse = EchoResponse <$> parseSString

-- | Unparse 'EchoResponse'
unparseEchoResponse :: EchoResponse -> Builder
unparseEchoResponse = unparseSString . unEchoResponse


-- | @\<get_assertions_response\> ::= ( \<term\>* )@
newtype GetAssertionsResponse = GetAssertionsResponse
  { unGetAssertionsResponse :: [Term] }
  deriving (Show, Read, Eq)

-- | Parse 'GetAssertionsResponse'
parseGetAssertionsResponse :: Parser GetAssertionsResponse
parseGetAssertionsResponse = do
  par '('
  terms <- many' parseTerm
  par ')'
  return $ GetAssertionsResponse terms

-- | Unparse 'GetAssertionsResponse'
unparseGetAssertionsResponse :: GetAssertionsResponse -> Builder
unparseGetAssertionsResponse (GetAssertionsResponse terms) = unwordsB
  [ char8 '('
  , unwordsB $ unparseTerm <$> terms
  , char8 ')'
  ]


-- | @\<get_assignment_response\> ::= ( \<t_valuation_pair\>* )@
newtype GetAssignmentResponse = GetAssignmentResponse
  { unGetAssignmentResponse :: [TValuationPair] }
  deriving (Show, Read, Eq)

-- | Parse 'GetAssignmentResponse'
parseGetAssignmentResponse :: Parser GetAssignmentResponse
parseGetAssignmentResponse = do
  par '('
  tValuationPairs <- many' parseTValuationPair
  par ')'
  return $ GetAssignmentResponse tValuationPairs

-- | Unparse 'GetAssignmentResponse'
unparseGetAssignmentResponse :: GetAssignmentResponse -> Builder
unparseGetAssignmentResponse (GetAssignmentResponse tValuationPairs) = unwordsB
  [ char8 '('
  , unwordsB $ unparseTValuationPair <$> tValuationPairs
  , char8 ')'
  ]


-- | @\<get_info_response\> ::= ( \<info_response\>+ )@
newtype GetInfoResponse = GetInfoResponse
  { unGetInfoResponse :: NonEmpty InfoResponse }
  deriving (Show, Read, Eq)

-- | Parse 'GetInfoResponse'
parseGetInfoResponse :: Parser GetInfoResponse
parseGetInfoResponse = do
  par '('
  infoResponses <- NE.fromList <$> many1' parseInfoResponse
  par ')'
  return $ GetInfoResponse infoResponses

-- | Unparse 'GetInfoResponse'
unparseGetInfoResponse :: GetInfoResponse -> Builder
unparseGetInfoResponse (GetInfoResponse infoResponses) = unwordsB 
  [ char8 '('
  , unwordsB $ unparseInfoResponse <$> NE.toList infoResponses
  , char8 ')'
  ] 


-- | @\<get_model_response\> ::= ( \<model_response\>* )@
newtype GetModelResponse = GetModelResponse
  { unGetModelResponse :: [ModelResponse] }
  deriving (Show, Read, Eq)

-- | Parse 'GetModelResponse'
parseGetModelResponse :: Parser GetModelResponse
parseGetModelResponse = do
  par '('
  modelResponses <- many' parseModelResponse
  par ')'
  return $ GetModelResponse modelResponses

-- | Unparse 'GetModelResponse'
unparseGetModelResponse :: GetModelResponse -> Builder
unparseGetModelResponse (GetModelResponse modelResponses) = unwordsB
  [ char8 '('
  , unwordsB $ unparseModelResponse <$> modelResponses
  , char8 ')'
  ]


-- | @\<get_option_response\> ::= \<attribute_value\>@
newtype GetOptionResponse = GetOptionResponse
  { unGetOptionResponse :: AttributeValue }
  deriving (Show, Read, Eq)

-- | Parse 'GetOptionResponse'
parseGetOptionResponse :: Parser GetOptionResponse
parseGetOptionResponse = GetOptionResponse <$> parseAttributeValue

-- | Unparse 'GetOptionResponse'
unparseGetOptionResponse :: GetOptionResponse -> Builder
unparseGetOptionResponse = unparseAttributeValue . unGetOptionResponse


-- | @\<get_proof_response\> ::= \<s_expr\>@
newtype GetProofResponse = GetProofResponse
  { unGetProofResponse :: SExpr }
  deriving (Show, Read, Eq)

-- | Parse 'GetProofResponse'
parseGetProofResponse :: Parser GetProofResponse
parseGetProofResponse = GetProofResponse <$> parseSExpr

-- | Unparse 'GetProofResponse'
unparseGetProofResponse :: GetProofResponse -> Builder
unparseGetProofResponse = unparseSExpr . unGetProofResponse


-- | @\<get_unsat_assumptions_response\> ::= ( \<symbol\>* )@
newtype GetUnsatAssumptionsResponse = GetUnsatAssumptionsResponse
  { unGetUnsatAssumptionsResponse :: [Symbol] }
  deriving (Show, Read, Eq)

-- | Parse 'GetUnsatAssumptionsResponse'
parseGetUnsatAssumptionsResponse :: Parser GetUnsatAssumptionsResponse
parseGetUnsatAssumptionsResponse = do
  par '('
  symbols <- many' parseSymbol
  par ')'
  return $ GetUnsatAssumptionsResponse symbols

-- | Unparse 'GetUnsatAssumptionsResponse'
unparseGetUnsatAssumptionsResponse :: GetUnsatAssumptionsResponse -> Builder
unparseGetUnsatAssumptionsResponse (GetUnsatAssumptionsResponse r) = unwordsB
  [ char8 '('
  , unwordsB $ unparseSymbol <$> r
  , char8 ')'
  ]


-- | @\<get_unsat_core_response\> ::= ( \<symbol\>* )@
newtype GetUnsatCoreResponse = GetUnsatCoreResponse
  { unGetUnsatCoreResponses :: [Symbol] }
  deriving (Show, Read, Eq)

-- | Parse 'GetUnsatCoreResponse'
parseGetUnsatCoreResponse :: Parser GetUnsatCoreResponse
parseGetUnsatCoreResponse = do
  par '('
  symbols <- many' parseSymbol
  par ')'
  return $ GetUnsatCoreResponse symbols

-- | Unparse 'GetUnsatCoreResponse'
unparseGetUnsatCoreResponse :: GetUnsatCoreResponse -> Builder
unparseGetUnsatCoreResponse (GetUnsatCoreResponse symbols) = unwordsB
  [ char8 '('
  , unwordsB $ unparseSymbol <$> symbols
  , char8 ')'
  ]


-- | @\<get_value_response\> ::= ( \<valuation_pair\>+ )@
newtype GetValueResponse = GetValueResponse
  { unGetValueResponse :: NonEmpty ValuationPair }
  deriving (Show, Read, Eq)

-- | Parse 'GetValueResponse'
parseGetValueResponse :: Parser GetValueResponse
parseGetValueResponse = do
  par '('
  valuationPairs <- NE.fromList <$> many1' parseValuationPair
  par ')'
  return $ GetValueResponse valuationPairs

-- | Unparse 'GetValueResponse'
unparseGetValueResponse :: GetValueResponse -> Builder
unparseGetValueResponse (GetValueResponse valuationPairs) = unwordsB
  [ char8 '('
  , unwordsB $ unparseValuationPair <$> NE.toList valuationPairs
  , char8 ')'
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
parseSpecificSuccessResponse :: Command -> Parser SpecificSuccessResponse
parseSpecificSuccessResponse = \case
  CheckSat ->
    SpecificSuccessResponseCheckSatResponse <$> parseCheckSatResponse
  CheckSatAssuming _ ->
    SpecificSuccessResponseCheckSatResponse <$> parseCheckSatResponse
  Echo _ ->
    SpecificSuccessResponseEchoResponse <$> parseEchoResponse
  GetAssertions ->
    SpecificSuccessResponseGetAssertionsResponse <$> parseGetAssertionsResponse
  GetAssignment ->
    SpecificSuccessResponseGetAssignmentResponse <$> parseGetAssignmentResponse
  GetInfo _ ->
    SpecificSuccessResponseGetInfoResponse <$> parseGetInfoResponse
  GetModel ->
    SpecificSuccessResponseGetModelResponse <$> parseGetModelResponse
  GetOption _ ->
    SpecificSuccessResponseGetOptionResponse <$> parseGetOptionResponse
  GetProof ->
    SpecificSuccessResponseGetProofResponse <$> parseGetProofResponse
  GetUnsatAssumptions ->
    SpecificSuccessResponseGetUnsatAssumptionsResponse <$> parseGetUnsatAssumptionsResponse
  GetUnsatCore ->
    SpecificSuccessResponseGetUnsatCoreResponse <$> parseGetUnsatCoreResponse
  GetValue _ ->
    SpecificSuccessResponseGetValueResponse <$> parseGetValueResponse
  _ -> fail "Command does not have a specific success response."

-- | Unparse 'SpecificSuccessResponse'
unparseSpecificSuccessResponse :: SpecificSuccessResponse -> Builder
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
    GeneralResponseError SString
  deriving (Show, Read, Eq)

-- | Parse 'GeneralResponse'
parseGeneralResponse :: Command -> Parser GeneralResponse
parseGeneralResponse command = choice
  [ GeneralResponseSuccess <$ string "success"
  , GeneralResponseSpecificSuccessResponse <$> parseSpecificSuccessResponse command
  , GeneralResponseUnsupported <$ string "unsupported"
  , parseGeneralResponseError
  ] <* skipSpace
  where
    parseGeneralResponseError = do
      par '('
      "error" *> skipSpace
      str <- parseSString
      par ')'
      return $ GeneralResponseError str

-- | Unparse 'GeneralResponse'
unparseGeneralResponse :: GeneralResponse -> Builder
unparseGeneralResponse = \case
  GeneralResponseSuccess -> byteString "success"
  GeneralResponseSpecificSuccessResponse specificSuccessResponse ->
    unparseSpecificSuccessResponse specificSuccessResponse
  GeneralResponseUnsupported -> byteString "unsupported"
  GeneralResponseError str ->
    unwordsB
      [ char8 '('
      , byteString "error"
      , unparseSString str
      , char8 ')'
      ]


syntaxTests :: IO Bool
syntaxTests = checkSequential $$(discover)
