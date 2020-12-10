# One programming language
This repo contains the one interpreter. The name one comes from the fact that all functions and variables are defined as a single character.

## Features
- recursion
- mathematical operations *, /, + and -
- mathematical order of operations
- show syntax errors before running
- stacktrace on error
- stackdump on error
- option to view the stack after execution
- unicode support (visually not optimal when debugging)
- decently fast

## Limitations
- only integers can be used
- output cant be negative (unsigned integer is used as output)
- all variable and function names have to consist of a single character

## How to
Build/run using the following command:
```cabal run one programs/a.one```

If you want to see the stack at the end of the program, with all variables and all created abstract syntax tree's.
Run the program with debug at the end:
```cabal run one programs/a.one debug```

To build the docs use:
```cabal haddock one```

## Example programs
There are multiple test programs in the programs folder.
- a shows most functionalities with a lot of comments
- c shows asian characters being used
- e shows emojis being used
- f calculates numbers in the fibonacci sequence
- l shows a recursive loop
- p contains an assignment of an undefined variable. This is illegal, crashes the interpreter and shows the stacktrace and stackdump.
- x shows almost all unicode characters being used

## Video explanation
https://youtu.be/xAcbNWbwMI4


