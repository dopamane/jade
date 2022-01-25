module Jade.Expression (
    readExpr,
    Expr (..),
) where

import Data.Char (
    isSpace,
 )
import Data.Text (
    Text,
 )
import Data.Void (
    Void,
 )
import Text.Megaparsec (
    ParseErrorBundle,
    Parsec,
    empty,
    (<|>),
 )
import qualified Text.Megaparsec as Parser
import qualified Text.Megaparsec.Char as Parser
import qualified Text.Megaparsec.Char.Lexer as Lexer

data Expr
    = Atom !Text
    | List ![Expr]
    deriving (Show, Read, Eq)

type Parser = Parsec Void Text

parseExprSpace :: Parser ()
parseExprSpace = Lexer.space Parser.space1 skipLineComment empty
{-# INLINE parseExprSpace #-}

skipLineComment :: Parser ()
skipLineComment = Lexer.skipLineComment ";"
{-# INLINE skipLineComment #-}

-- | Basic S-expression parser.
parseExpr :: Parser Expr
parseExpr = parseAtom <|> parseList
  where
    parseAtom :: Parser Expr
    parseAtom = lexeme (Atom <$> Parser.takeWhile1P Nothing notSpecial)

    parseList :: Parser Expr
    parseList =
        List <$> (lparen *> Parser.many parseExpr <* rparen)

    special :: Char -> Bool
    special c = isSpace c || c == '(' || c == ')' || c == ';'

    notSpecial :: Char -> Bool
    notSpecial = not . special

    lparen :: Parser Char
    lparen = lexeme $ Parser.char '('

    rparen :: Parser Char
    rparen = lexeme $ Parser.char ')'

    lexeme :: Parser a -> Parser a
    lexeme = Lexer.lexeme parseExprSpace

readExpr :: String -> Text -> Either (ParseErrorBundle Text Void) Expr
readExpr = Parser.parse parseExpr
