module Parser where

import Control.Monad    
import Control.Applicative
import Data.Char

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

-- Higher level functions

runParser :: Parser from to -> [from] -> to
runParser (Parser fun) toks = case fun toks of
    [(r, [])] -> r
    _ -> error "Parser couldn't consume entire stream"