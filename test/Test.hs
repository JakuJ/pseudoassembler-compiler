module Main (main) where 

import System.Process(callProcess)
import System.Directory(listDirectory)
import Control.Monad(forM_)
import Control.Applicative((<$>))

import Lexer
import Parser
import Translation

test_file :: FilePath -> IO ()
test_file sourceName = do
    source <- readFile sourceName
    -- make an AST and generate C code
    let generated_code = translateProgram . parseProgram . lexProgram $ source
    -- write C code to file
    writeFile "test.c" generated_code
    -- compile C to machine code
    callProcess "gcc" ["-O3", "-o", "test.out", "test.c"]
    -- run output
    callProcess "./test.out" []
    -- remove C file
    callProcess "rm" ["test.c", "test.out"]

main :: IO ()
main = do
    xs <- filter (\x -> (takeWhile (/= '.') . reverse) x == "aph") <$> listDirectory "./test/Programs"
    forM_ xs $ \x -> do
        print $ "TESTING" ++ x
        test_file . ("./test/Programs/" ++ ) $ x