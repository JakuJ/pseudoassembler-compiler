module Translation (
    translateProgram
) where

import Data.Maybe(fromJust)
import Parser(Tree(..), AddressNode(..))

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

translate' :: Int -> Tree -> String -- ~spaghetti~
translate' i t = case t of
    Epsilon -> ""
    LineNode s t1@(DeclNode _ size) t2 -> (if null s then "" else s ++ ":;\n") ++ translate' i t1 ++ translate' (i + size) t2
    LineNode s t1@(DefNode _ size _) t2 -> (if null s then "" else s ++ ":;\n") ++ translate' i t1 ++ translate' (i + size) t2
    LineNode s t1 t2 -> (if null s then "" else s ++ ":;\n") ++ translate' i t1 ++ translate' i t2
    DeclNode n _ -> "#define " ++ n ++ " " ++ show i ++ "\n"
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

translateProgram :: Tree -> String
translateProgram = encloseRuntime . translate' 0