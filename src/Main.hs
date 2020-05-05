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
import Runner
import GHC.IO.Encoding

main = do
    --parse command line argument
    setLocaleEncoding utf8
    args <- getArgs
    let debug = lastArgIsDebug args
    let firstArgErr = getFirstArg args
    if isError firstArgErr
        then do
            putStrLn $ getError firstArgErr
            exitFailure
        else return ()
    let firstArg = getValue firstArgErr

    --read given file
    let fileContentErr = readOneFile firstArg
    if isError fileContentErr
        then do
            putStrLn $ getError fileContentErr
            exitFailure
        else return ()
    fileContent <- getValue fileContentErr

    --remove whitespaces and empty lines
    let cleanedInputErr = cleanInput fileContent
    if isError cleanedInputErr
        then do
            putStrLn $ getError cleanedInputErr
            exitFailure
        else return ()
    let cleanedInput = getValue cleanedInputErr

    let stackErr = createAST cleanedInput
    if isError stackErr
        then do
            putStrLn $ "input text:\n" ++ (show cleanedInput)
            putStrLn $ getError stackErr

            exitFailure
        else return ()
    let stack = getValue stackErr

    let programResult = run stack
    if isError $ fst programResult
        then do
            putStrLn $ getError $ fst programResult
            putStrLn $ "stackdump:\n" ++ (prettyStrStack $ snd programResult)
            exitFailure
        else if debug
            then putStrLn $ prettyStrStack $ snd programResult
            else return ()
    putStrLn $ show $ getValue $ fst programResult