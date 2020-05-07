Build/run using the following command:
cabal run one programs/a.one

To build the docs use:
cabal haddock one

There are multiple test programs in the programs folder.
- a shows most functionalities with a lot of comments
- c shows asian characters being used
- e shows emojis being used
- f calculates numbers in the fibonacci sequence
- l shows a recursive loop
- p contains an assignment of an undefined variable. This is illegal, crashes the interpreter and shows the stacktrace and stackdump.
- x shows almost all unicode characters being used

If you want to see the stack at the end of the program, with all variables and all created abstract syntax tree's.
Run the program with debug at the end, as shown below:
cabal run one programs/a.one debug

Features
- recursion
- fast
- mathematical operations *, /, + and -
- mathematical order of operations
- stacktrace on error
- stackdump on error
- option to view stack at end of program