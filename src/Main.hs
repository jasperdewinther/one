-- |The start of the program.
module Main where
import System.Environment
import Reader
import AST
import Data.Maybe
import System.Exit 
import Data.List
import Data.Char
import Data.Typeable
import Control.Monad.IO.Class
import Stack
import Tools
import Runner
import GHC.IO.Encoding
import System.Mem
import Control.Monad

-- |Exit program with error code 4294967295
myUnexpectedExit :: IO a
myUnexpectedExit = exitWith $ ExitFailure 4294967295

-- |Run program and print result.
main = do
    --limit to 4GB of memory
    setAllocationCounter 4000000000
    enableAllocationLimit
    --parse command line argument
    setLocaleEncoding utf8
    args <- getArgs
    --check if debug info should be printed
    let debug = lastArgIsDebug args
    let firstArgErr = getFirstArg args
    Control.Monad.when (isError firstArgErr)
        $ do putStrLn $ getError firstArgErr
             myUnexpectedExit
    let firstArg = getValue firstArgErr

    --read given file
    let fileContentErr = readOneFile firstArg
    Control.Monad.when (isError fileContentErr)
        $ do putStrLn $ getError fileContentErr
             myUnexpectedExit
    fileContent <- getValue fileContentErr

    --remove whitespaces and empty lines
    let cleanedInputErr = cleanInput fileContent
    Control.Monad.when (isError cleanedInputErr)
        $ do putStrLn $ getError cleanedInputErr
             myUnexpectedExit
    let cleanedInput = getValue cleanedInputErr

    let stackErr = createAST cleanedInput
    Control.Monad.when (isError stackErr)
        $ do putStrLn $ "input text:\n" ++ show cleanedInput
             putStrLn $ getError stackErr
             myUnexpectedExit
    let stack = getValue stackErr

    let programResult = run stack
    if isError $ fst programResult
        --handle potential errors
        then do
            putStrLn $ getError $ fst programResult
            putStrLn $ "stackdump:\n" ++ prettyStrStack (snd programResult)
            myUnexpectedExit
        else Control.Monad.when debug
            --if debug info should be printed, print stack
                $ do putStrLn $ prettyStrStack $ snd programResult
    print $ getValue $ fst programResult
    exitWith $ ExitFailure $ getValue $ fst programResult