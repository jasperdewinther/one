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
    print newStack
    print $ setStack 'a' (ConstIntNode 1) newStack
    print $ getFromStack 'a' $ setStack 'a' (ConstIntNode 1) newStack
    print $ createAST $ cleanInput fileContent
    print $ createFunctionAST $ snd $ head $ createAST $ cleanInput fileContent