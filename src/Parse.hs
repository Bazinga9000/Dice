{-# LANGUAGE FlexibleContexts #-}
module Parse where

import Control.Applicative((<*))
import Control.Monad
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Number
import Text.Parsec.Expr
import Text.Parsec.Token
import Text.Parsec.Language
import qualified Text.Parsec.Expr as E
import Expr

-- Helper Functions

lexemeP :: Parser a -> Parser a
lexemeP p = p <* whitespace

whitespace :: Parser ()
whitespace = void $ many $ char ' '


parenthesize :: Parser Expr -> Parser Expr
parenthesize p = do
    void $ lexemeP $ char '('
    e <- lexemeP p
    void $ lexemeP $ char ')'
    return $ Parens e

numericExpr :: Parser Expr
numericExpr = do
    n <- lexemeP $ fractional3 True
    return $ Number n Constant


identifierString :: Parser String
identifierString = lexemeP $ many1 (char '$' <|> upper)

identifierExpr :: Parser Expr
identifierExpr = Identifier <$> identifierString

appExpr :: Parser Expr
appExpr = do
    f <- identifierExpr <|> parenthesize parseExpr
    void $ lexemeP $ char '('
    args <- lexemeP $ sepBy parseExpr (char ',')
    void $ lexemeP $ char ')'
    return $ App f args

lambdaExpr :: Parser Expr
lambdaExpr = do
    void $ lexemeP $ char '\\'
    args <- lexemeP $ sepBy identifierString (char ',')
    void $ lexemeP $ string "->"
    Function args <$> parseExpr

ifExpr :: Parser Expr
ifExpr = do
    void $ lexemeP $ string "if"
    cond <- parseExpr
    void $ lexemeP $ string "then"
    bl <- parseExpr
    void $ lexemeP $ string "else"
    If cond bl <$> parseExpr

relationalSectionExpr :: Parser Expr 
relationalSectionExpr = do
--    void $ char '('
    op <- lexemeP $ choice $ map string ["=","/=","<",">",">=","<="]
--    void $ char ')'
    Function ["$"] . BinOp op (Identifier "$") <$> parseExpr

parseExpr :: Parser Expr
parseExpr = whitespace >> E.buildExpressionParser table term where
    table = [[prefix "-"]
        ,[binary "^" E.AssocLeft]
        ,[binary "*" E.AssocLeft, binary "/" E.AssocLeft, binary "%" E.AssocLeft]
        ,[binary "+" E.AssocLeft, binary "-" E.AssocLeft]
        ,[binary "=" E.AssocNone, binary "/=" E.AssocNone, binary "<" E.AssocNone, binary ">" E.AssocNone, binary ">=" E.AssocNone, binary "<=" E.AssocNone]
        ,[prefix "not"]
        ,[binary "and" E.AssocLeft]
        ,[binary "or" E.AssocLeft]
        ]
    binary name = E.Infix (BinOp name <$ lexemeP (string name))
    prefix name = E.Prefix (UnaryOp name <$ lexemeP (string name))
    term = try appExpr <|> ifExpr <|> numericExpr <|> try relationalSectionExpr <|> identifierExpr <|> lambdaExpr <|> parenthesize parseExpr
