module Main where
import System.Environment
import Reader
import Data.Maybe
import System.Exit (exitFailure)
import Data.List.Split


main = do
    args <- getArgs
    if length args == 0
        then do
            putStrLn "no input file given"
            exitFailure
        else return ()
    let fileContent = readOneFile $ args !! 0
    if isNothing fileContent
        then do
            putStrLn "input filename is not correct" 
            exitFailure
        else return ()
    realfileContent <- fromJust fileContent
    let commands = splitOn "\n" realfileContent
    print commands
    