{-# LANGUAGE LambdaCase #-}

module Lexer (
    Token(..),
    lexProgram
) where

import Data.List (groupBy, intercalate)
import Data.Char (isSpace, isDigit, isUpper)

-- LEXING: First pass, "classical approach"
-- Grouping text, number and symbol tokens

data Lexeme = Text String
            | Number Int
            | Symbol Char
    deriving (Show)

symbols :: String
symbols = [',', '*', '(', ')']

isNewLine, isSymbol, isIdentificator, isNumberStart :: Char -> Bool
isNewLine = (== '\n')
isSymbol = (`elem` symbols)
isIdentificator x = isUpper x || x == '_'
isNumberStart x = isDigit x || x == '-'

lexer :: String -> Int -> [(Lexeme, Int)]
lexer (c:cs) ln
    | isNewLine c = lexer cs (ln + 1)
    | isSpace c = lexer cs ln
    | isSymbol c = (Symbol c, ln) : lexer cs ln
    | isNumberStart c = (Number (read (c : numbers) :: Int), ln) : lexer next ln
        where
            numbers = takeWhile isDigit cs
            next = dropWhile isDigit cs

lexer (c:cs) ln
    | isIdentificator c = (Text (c : letters), ln) : lexer next ln
        where
            letters = takeWhile isIdentificator cs
            next = dropWhile isIdentificator cs

lexer ('#': cs) ln = lexer next ln
    where
        next = dropWhile (not . isNewLine) cs

lexer [] _ = []
lexer (c:_) ln = error $ "Syntax error on line " ++ show ln ++ ": invalid character '" ++ c : "'"

pass1 :: String -> [(Lexeme, Int)]
pass1 = (`lexer` 1)

-- LEXING: Second pass (this can be done in one pass, but I do this for fun)
-- annotating keywords, labels and symbols

data Token  = Label String
            | Keyword String
            | Command String
            | Integer Int
            | Comma 
            | Asterisk 
            | Oparen 
            | Cparen
            | Newline
    deriving (Show, Eq)

keywords :: [String]
keywords = ["DS", "DC", "INTEGER"]

commands :: [String]
commands = ["L", "LA", "LR", "ST", "A", "AR", "S", "SR", "M", "MR", "D", "DR", "C", "CR", "J", "JN", "JP", "JZ"]

lexer2 :: Lexeme -> Token
lexer2 = \case
    Number i -> Integer i
    Symbol ',' -> Comma
    Symbol '*' -> Asterisk
    Symbol '(' -> Oparen
    Symbol ')' -> Cparen
    Text s | s `elem` keywords -> Keyword s
    Text s | s `elem` commands -> Command s
    Text s -> Label s

pass2 :: [(Lexeme, Int)] -> [(Token, Int)]
pass2 ls = (++ [(Newline, 0)]) . intercalate [(Newline, 0)] . groupBy (\(_, a) (_, b) -> a == b) $ [(lexer2 l, i) | (l, i) <- ls]

lexProgram :: String -> [Token]
lexProgram = map fst . pass2 . pass1