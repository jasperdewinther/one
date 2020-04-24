module Main where
import System.Environment
import Reader
import AST
import Data.Maybe
import System.Exit (exitFailure)
import Data.List
import Data.Char
import Data.Typeable
import Control.Monad.IO.Class
import Stack
import Tools

getFirstArg :: [String] -> MaybeError String
getFirstArg s = if length s == 0
                    then Error "no input file given"
                    else NotError $ head s

main = do
    --parse command line argument
    args <- getArgs
    let firstArgErr = getFirstArg args
    if isError firstArgErr
        then do
            putStrLn $ getError firstArgErr
            exitFailure
        else print $ "parsing file: " ++ (getValue firstArgErr)
    let firstArg = getValue firstArgErr

    --read given file
    let fileContentErr = readOneFile firstArg
    if isError fileContentErr
        then do
            putStrLn $ getError fileContentErr
            exitFailure
        else print "read input succesfully"
    fileContent <- getValue fileContentErr

    --remove whitespaces and empty lines
    let cleanedInputErr = cleanInput fileContent
    if isError cleanedInputErr
        then do
            putStrLn $ getError cleanedInputErr
            exitFailure
        else print "cleaned input succesfully"
    let cleanedInput = getValue cleanedInputErr

    print $ cleanedInput
    putStrLn $ prettyStrStack $ createAST cleanedInput
    let stack = createAST cleanedInput
    --if isError stackErr
    --    then do
    --        putStrLn $ getError stackErr
    --        exitFailure
    --    else print "read input succesfully"
    --let stack = getValue stackErr

    let executionResult = executeStackNode (getValue $ getFromStack 'a' stack ) stack

    if isError$ fst executionResult
        then do
            putStrLn $ getError $ fst executionResult
            exitFailure
        else print "execution succesfull"
    putStrLn $ prettyStrStack $ snd executionResult
    --print $ createAST $ cleanInput fileContent