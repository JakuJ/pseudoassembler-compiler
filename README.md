# pseudoassembler-compiler
A compiler written for prof. Homenda's pseudoassembly language.
Done entirely in Haskell using parsing combinators and some redundant lexing phases.

## Formal grammar:

    program ->  lines

    lines   ->  lines line
            |   Epsilon

    line    ->  Label allocation '\n'
            |   Label command '\n'
            |   Label '\n'

    allocation  ->  declaration
                |   definition

    declaration ->  'DS' 'INTEGER'
                |   'DS' Number '*' 'INTEGER'

    definition  ->  'DC' 'INTEGER' '(' Number ')'
                |   'DC' Number '*' 'INTEGER' '(' Number ')'

    command ->  move
            |   arithmetic
            |   compare
            |   jump

    move    ->  'L' Register ',' address
            |   'LA' Register ',' address
            |   'LR' Register ',' address
            |   'ST' Register ',' address

    arithmetic  ->  'A' Register ',' address
                |   'S' Register ',' address
                |   'M' Register ',' address
                |   'D' Register ',' address
                |   'AR' Register ',' address
                |   'SR' Register ',' address
                |   'MR' Register ',' address
                |   'DR' Register ',' address

    compare ->  'C' Register ',' address
            |   'CR' Register ',' address

    jump    ->  'J' Label
            |   'JZ' Label
            |   'JN' Label
            |   'JP' Label

    address ->  Label
            |   Register
            |   Register '(' Number ')'
            |   Number

    Register    -> 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | 11 | 12 | 13 | 14 | 15
    Number  -> (any non-negative number)
    Label   -> (any uppercase identifier)
