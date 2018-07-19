{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Control.Monad (when, unless)
import System.Process (callProcess)
import System.Environment (getArgs, getProgName)
import Control.Exception (catch, SomeException)

import Lexer
import Parser
import Translation

-- HELPER FUNCTIONS

printTree :: Tree -> IO ()
printTree Epsilon = return ()
printTree (LineNode l c nl) = putStrLn (show l ++ "\t" ++ show c) >> printTree nl

-- ENTRY POINT

main :: IO ()
main = catch (do
    -- interface
    args' <- getArgs
    let sourceName = last args'
    let args = init args'
    source <- readFile sourceName
    let programName = takeWhile (/='.') sourceName
    -- make an AST and generate C code
    let tokens = lexProgram source
    when ("-v" `elem` args) $ mapM_ print tokens
    let ast = parseProgram tokens
    when ("-v" `elem` args) $ printTree ast
    let generated_code = translateProgram ast
    -- write C code to file
    let execName = if "-o" `elem` args then dropWhile (/="-o") args !! 1 else programName ++ ".out"
    writeFile (programName ++ ".c") generated_code
    -- compile C to machine code
    callProcess "gcc" ["-O3", "-o", execName, programName ++ ".c"]
    -- remove C file
    unless ("-S" `elem` args) $ callProcess "rm" [programName ++ ".c"]
        ) $ \(err :: SomeException) -> do
                print err
                name <- getProgName
                putStrLn $ "\nUsage:\n\t" ++ name ++ " [-o output] [-S] source"