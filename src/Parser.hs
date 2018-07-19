{-# LANGUAGE LambdaCase #-}

module Parser (
    Parser(Parser),
    Tree(..),
    AddressNode(..),
    parseProgram,
) where

import Control.Applicative
import Control.Monad    
import Lexer

-- PARSER TYPE USED FOR PARSER COMBINATORS

newtype Parser from to = Parser {parse :: [from] -> [(to, [from])]}

instance Functor (Parser from) where
    fmap f (Parser fun) = Parser $ \toks -> [(f a, b) | (a, b) <- fun toks]

instance Applicative (Parser from) where
    pure a = Parser $ \toks -> [(a, toks)]
    (Parser fun1) <*> (Parser fun2) = Parser $ \toks -> [(f a, toks2) | (f, toks1) <- fun1 toks, (a, toks2) <- fun2 toks1]

instance Monad (Parser from) where
    return = pure 
    (Parser p) >>= f = Parser $ \inp -> concat [parse (f tree) inp' | (tree, inp') <- p inp]

instance Alternative (Parser from) where
    empty = Parser $ const []
    (Parser p) <|> (Parser q) = Parser $ \toks -> case p toks of
        [] -> q toks
        res -> res

instance MonadPlus (Parser from) where
    mzero = empty
    (Parser p) `mplus` (Parser q) = Parser $ \inp-> p inp ++ q inp

-- DATA TYPES

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

parseSymbol :: Eq a => a -> Parser a a
parseSymbol x = Parser $ \case
    (t:toks) -> [(t, toks) | t == x]
    _ -> []

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

-- EXPORTED FUNCTIONS

runParser :: Parser from to -> [from] -> to
runParser (Parser fun) toks = case fun toks of
    [(r, [])] -> r
    _ -> error "Parser couldn't consume entire stream"
    
parseProgram :: [Token] -> Tree
parseProgram = runParser (assignMemoryLabels <$> parseLine)
