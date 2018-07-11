{-# LANGUAGE LambdaCase #-}

module HpaCompiler where

import Data.Char (isSpace, isDigit, isUpper)
import Control.Monad
import Control.Applicative
import Data.List
import Parser

-- LEXING: First pass
-- Grouping text, number and symbol tokens

data Lexeme = Text String
            | Number Int
            | Symbol Char
    deriving (Show)

symbols :: String
symbols = [',', '*', '(', ')']

isNewLine, isSymbol, isIdentificator :: Char -> Bool
isNewLine = (== '\n')
isSymbol = (`elem` symbols)
isIdentificator x = isUpper x || x == '_'

lexer :: String -> Int -> [(Lexeme, Int)]
lexer (c:cs) ln
    | isNewLine c = lexer cs (ln + 1)
    | isSpace c = lexer cs ln
    | isSymbol c = (Symbol c, ln) : lexer cs ln
    | isDigit c = (Number (read (c : numbers) :: Int), ln) : lexer next ln
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
pass1 s = lexer s 1

-- LEXING: Second pass
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
pass2 ls = (++ [(Newline, 0)]) . intercalate [(Newline, 0)] . groupBy (\(a,b) (c,d) ->b == d) $ [(lexer2 l, i) | (l, i) <- ls]

-- PARSING
    
data Tree =   LineNode String Tree Tree -- label, command or allocation, next line
            | DeclNode String Int -- size
            | DefNode String Int Int -- size, value
            | ArithmeticNode String Int AddressNode -- type, destination, source (label / absolute address / addressing register)
            | JumpNode String String -- type, target
            | Epsilon
                deriving (Show)
                
data AddressNode =    RegisterNode Int
                    | LabelNode String
                    | AbsoluteNode Int
                    | RelativeNode Int Int 
                        deriving (Show)
-- BASIC PARSERS

parseLabel :: Parser Token String
parseLabel = Parser $ \case
    (Label s : toks) -> [(s, toks)]
    _ -> []

parseKeyword :: String -> Parser Token String
parseKeyword s = Parser $ \case
    (Keyword k : toks) -> [(s, toks) | s == k]
    _ -> []

parseCommand :: Parser Token String
parseCommand = Parser $ \case
    (Command s : toks) -> [(s, toks)]
    _ -> []

parseSymbol :: Token -> Parser Token Token
parseSymbol s = Parser $ \case
    (t:toks) -> [(t, toks) | t == s]
    _ -> []

parseRegister :: Parser Token Int
parseRegister = Parser $ \case
    (Integer i : toks) -> [(i, toks) | i >= 0 && i <= 15]
    _ -> []

parseOffset :: Parser Token Int
parseOffset = Parser $ \case
    (Integer i : toks) -> [(i, toks)]
    _ -> []

parseLabelNode, parseRegisterNode, parseAbsoluteNode, parseRelativeNode :: Parser Token AddressNode
parseLabelNode = LabelNode <$> parseLabel
parseRegisterNode = RegisterNode <$> parseRegister
parseAbsoluteNode = AbsoluteNode <$> parseOffset
parseRelativeNode = do
    o <- parseOffset
    _ <- parseSymbol Oparen
    r <- parseRegister
    _ <- parseSymbol Cparen
    return $ RelativeNode o r

-- HIGHER LEVEL PARSERS

parseAddress :: Parser Token AddressNode
parseAddress = parseRelativeNode <|> parseRegisterNode <|> parseAbsoluteNode <|> parseLabelNode

parseDeclaration :: Parser Token Tree
parseDeclaration = do
    l <- parseLabel
    _ <- parseKeyword "DS"
    s <- (parseOffset >>= \x -> parseSymbol Asterisk >> return x) <|>return 1
    _ <- parseKeyword "INTEGER"
    return $ DeclNode l s

parseDefinition :: Parser Token Tree
parseDefinition = do
    l <- parseLabel
    _ <- parseKeyword "DC"
    s <- (parseOffset >>= \x -> parseSymbol Asterisk >> return x) <|>return 1
    _ <- parseKeyword "INTEGER"
    _ <- parseSymbol Oparen
    n <- parseOffset
    _ <- parseSymbol Cparen
    return $ DefNode l s n

parseArithmetic :: Parser Token Tree
parseArithmetic = do
    t <- parseCommand
    d <- parseRegister
    _ <- parseSymbol Comma
    a <- parseAddress
    return $ ArithmeticNode t d a

parseJump :: Parser Token Tree
parseJump = JumpNode <$> parseCommand <*> parseLabel

parseExpression :: Parser Token Tree
parseExpression = parseDeclaration <|> parseDefinition <|> parseArithmetic <|> parseJump

parseLine :: Parser Token Tree
parseLine = do
    l <- parseLabel <|> return ""
    c <- parseExpression <|> return Epsilon -- ! Unsafe way of handling trailing end-of-program labels
    _ <- parseSymbol Newline
    n <- parseLine <|> return Epsilon
    return $ LineNode l c n

-- TRANSLATION

encloseRuntime :: FilePath -> String
encloseRuntime prog = "#include <stdlib.h>\n\
\#include <stdio.h>\n\
\#define MEM_START 1000\n\
\int main(void)\n{\n\
\int* memory = (int*)calloc(1000, sizeof(int));\n\
\int compare = 0;\n\
\" ++ prog ++ "\nreturn 0;\n}\n"

translate :: Tree -> String
translate = \case
    Epsilon -> ""
    (LineNode s t1 t2) -> (if null s then "" else s ++ ":;\n") ++ translate t1 ++ translate t2

-- ENTRY POINT

tokenizeFile :: FilePath -> IO [Token]
tokenizeFile path = map fst . pass2 . pass1 <$> readFile path

printTree :: Tree -> IO ()
printTree Epsilon = return ()
printTree (LineNode l c nl) = putStrLn (show l ++ "\t" ++ show c) >> printTree nl

main :: IO ()
main = do
    tokens <- tokenizeFile "Programs/domowa.hpa"
    putStrLn "Kod po tokenizacji:"
    mapM_ print tokens

    putStrLn "\nDrzewo parsowania: "
    let tree = head . parse parseLine $ tokens
    printTree . fst $ tree