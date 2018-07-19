{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Data.Char (isSpace, isDigit, isUpper)
import Control.Monad
import Control.Applicative
import Data.List
import Data.Maybe (fromJust)
import System.Process (callProcess)
import System.Environment (getArgs, getProgName)
import Control.Exception
import Parser

-- LEXING: First pass, "classical approach"
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
isNumber x = isDigit x || x == '-'

lexer :: String -> Int -> [(Lexeme, Int)]
lexer (c:cs) ln
    | isNewLine c = lexer cs (ln + 1)
    | isSpace c = lexer cs ln
    | isSymbol c = (Symbol c, ln) : lexer cs ln
    | isNumber c = (Number (read (c : numbers) :: Int), ln) : lexer next ln
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
pass2 ls = (++ [(Newline, 0)]) . intercalate [(Newline, 0)] . groupBy (\(a,b) (c,d) -> b == d) $ [(lexer2 l, i) | (l, i) <- ls]

-- PARSING (parsing combinators approach)
    
data Tree =   LineNode String Tree Tree             -- label, command or allocation, next line
            | DeclNode String Int                   -- size
            | DefNode String Int Int                -- size, value
            | ArithmeticNode String Int AddressNode -- type, destination, source (label / absolute address / addressing register)
            | JumpNode String String                -- type, target
            | Epsilon
                deriving (Show)
                
data AddressNode =    RegisterNode Int
                    | LabelNode String
                    | AbsoluteNode Int
                    | RelativeNode Int Int 

instance Show AddressNode where
    show (RegisterNode i) = "registers[" ++ show i ++ "]"
    show (LabelNode s) = "memory[" ++ s ++ "]"
    show (AbsoluteNode i) = "memory[" ++ show ((i - 1000) `div` 4) ++ "]"
    show (RelativeNode off r) = "memory[(registers[" ++ show r ++ "] - 1000 + " ++ show off ++ ") / 4]"

-- PRIMITIVE PARSERS

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
    _ <- parseKeyword "DS"
    s <- (parseOffset >>= \x -> parseSymbol Asterisk >> return x) <|>return 1
    _ <- parseKeyword "INTEGER"
    return $ DeclNode "" s

parseDefinition :: Parser Token Tree
parseDefinition = do
    _ <- parseKeyword "DC"
    s <- (parseOffset >>= \x -> parseSymbol Asterisk >> return x) <|>return 1
    _ <- parseKeyword "INTEGER"
    _ <- parseSymbol Oparen
    n <- parseOffset
    _ <- parseSymbol Cparen
    return $ DefNode "" s n

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
    c <- parseExpression <|> return Epsilon -- ? Unsafe ? way of handling trailing end-of-program labels
    _ <- parseSymbol Newline
    n <- parseLine <|> return Epsilon
    return $ LineNode l c n

-- DISTINGUISH JUMP LABELS FROM MEMORY ALLOCATION ONES

assignMemoryLabels :: Tree -> Tree
assignMemoryLabels = \case
    LineNode l (DeclNode "" s) next -> LineNode "" (DeclNode l s) (assignMemoryLabels next)
    LineNode l (DefNode "" s v) next -> LineNode "" (DefNode l s v) (assignMemoryLabels next)
    LineNode l n next-> LineNode l n (assignMemoryLabels next)
    Epsilon -> Epsilon

-- TRANSLATION

encloseRuntime :: String -> String
encloseRuntime prog = "#include <stdlib.h>\n\
\#include <stdio.h>\n\
\int main(void)\n{\n\
\int * memory = (int*)calloc(1000, sizeof(int));\n\
\int * registers = (int*)calloc(16, sizeof(int));\n\
\registers[14] = 1000; registers[15] = 2000;\n\
\int flag = 0;\n\
\" ++ prog ++ "\n\
\printf(\"REGISTER DUMP:\\n\");\n\
\for(int i = 0; i < 16; i++){printf(\"%d:\\t%d\\n\", i, registers[i]);}\n\
\printf(\"MEMORY DUMP:\\n\");\n\
\for(int i = 0; i < 1000; i++){if (memory[i] != 0) {printf(\"%d:\\t%d\\n\", 1000 + 4 * i, memory[i]);}}\n\
\return 0;\n}\n"

encloseForLoop :: Int -> String -> String
encloseForLoop i s = "for(int i = 0; i < " ++ show i ++ "; i++){" ++ s ++"}\n"

translate :: Tree -> String
translate = translate' 0

translate' :: Int -> Tree -> String -- ~spaghetti~
translate' i t = case t of
    Epsilon -> ""
    LineNode s t1@(DeclNode _ size) t2 -> (if null s then "" else s ++ ":;\n") ++ translate' i t1 ++ translate' (i + size) t2
    LineNode s t1@(DefNode _ size _) t2 -> (if null s then "" else s ++ ":;\n") ++ translate' i t1 ++ translate' (i + size) t2
    LineNode s t1 t2 -> (if null s then "" else s ++ ":;\n") ++ translate' i t1 ++ translate' i t2
    DeclNode n s -> "#define " ++ n ++ " " ++ show i ++ "\n"
    DefNode n s v -> "#define " ++ n ++ " " ++ show i ++ "\n" ++ encloseForLoop s ("memory[i + " ++ show i ++ "] = " ++ show v ++ ";")
    ArithmeticNode comm dest src -> case comm of
        "LA" -> "registers[" ++ show dest ++ "] = " ++ (\(LabelNode s) -> s) src ++ " * 4 + 1000; // LA\n"
        "ST" -> show src ++ " = registers[" ++ show dest ++ "]; // ST\n"
        f | f `elem` ["L", "LR"] -> "registers[" ++ show dest ++ "] = " ++ show src ++ "; // " ++ f ++ "\n"
        f | f `elem` ["A", "S", "M", "D", "AR", "SR", "MR", "DR"] ->
            let s = fromJust $ lookup f [("A", "+"), ("S", "-"), ("M", "*"), ("D", "/"), ("AR", "+"), ("SR", "-"), ("MR", "*"), ("DR", "/")] in 
                "registers[" ++ show dest ++ "] " ++ s ++ "= " ++ show src ++ "; // " ++ f ++ "\n"
        f | f `elem` ["C", "CR"] -> "flag = registers[" ++ show dest ++ "] - " ++ show src ++ "; // " ++ f ++ "\n"
    JumpNode f label -> case f of
        "J" -> "goto " ++ label ++ "; // J\n"
        "JN" -> "if (flag < 0) {goto " ++ label ++ ";} // JN \n"
        "JZ" -> "if (flag == 0) {goto " ++ label ++ ";} // JZ \n" 
        "JP" -> "if (flag > 0) {goto " ++ label ++ ";} // JP \n"

-- HELPER FUNCTIONS

tokenizeFile :: String -> [Token]
tokenizeFile = map fst . pass2 . pass1

printTree :: Tree -> IO ()
printTree Epsilon = return ()
printTree (LineNode l c nl) = putStrLn (show l ++ "\t" ++ show c) >> printTree nl

-- ENTRY POINT

main :: IO () -- TODO: test all sample programs
main = catch (do
    -- interface
    args' <- getArgs
    let sourceName = last args'
    let args = init args'
    source <- readFile sourceName
    let programName = takeWhile (/='.') sourceName
    -- make an AST and generate C code
    let tokens = tokenizeFile source
    when ("-v" `elem` args) $ mapM_ print tokens
    let ast =  assignMemoryLabels . runParser parseLine $ tokens
    when ("-v" `elem` args) $ printTree ast
    let generated_code = encloseRuntime . translate $ ast
    -- write C code to file
    let execName = if "-o" `elem` args then dropWhile (/="-o") args !! 1 else programName ++ ".out"
    writeFile (programName ++ ".c") generated_code
    -- compile C to machine code
    callProcess "gcc" ["-O3", "-o", execName, programName ++ ".c"]
    -- remove C file
    unless ("-S" `elem` args) $ callProcess "rm" [programName ++ ".c"]
    -- run compiled program
    callProcess "./test" []
        ) $ \(err :: SomeException) -> do
                print err
                name <- getProgName
                putStrLn $ "\nUsage:\n\t" ++ name ++ " [-o output] [-S] source"