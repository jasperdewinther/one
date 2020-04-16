module Main where
import System.Environment
import Reader
import LST
import Data.Maybe
import System.Exit (exitFailure)
import Data.List
import Data.Char
import Data.Typeable
import Control.Monad.IO.Class



main = do
    --parse command line argument
    args <- getArgs
    if Prelude.length args == 0
        then do
            putStrLn "no input file given"
            exitFailure
        else return ()
    --read given file
    fileContent <- readOneFile $ args !! 0
    print $ cleanInput fileContent
    print $ cleanInput fileContent
    print $ lookup 'a' newStack
    print newStack
    print $ setStack 'b' (VariableNode 1) newStack