-- |This module contains more general functions and is used in almost all other modules.
module Tools where

import Data.Char

-- |Data that contains either a value or an error.
-- This is used heavily as backtrace for errors.
data MaybeError a = NotError a | Error String

-- |Check if a MaybeError is a value and not an error.
isValue :: MaybeError a -> Bool
isValue (NotError _) = True
isValue _ = False

-- |Get the value from a MaybeError.
-- Note that this will fail if the MaybeError was an Error.
getValue :: MaybeError a -> a
getValue (NotError m) = m

-- |Check if a MaybeError is an error and not a value.
isError :: MaybeError a -> Bool
isError (Error _) = True
isError _ = False

-- |Get the error from a MaybeError.
-- Note that this will fail if the MaybeError was a Value.
getError :: MaybeError a -> String
getError (Error m) = m

-- |Removes elements from list that return true on given function.
conditionalRemove :: [a] -> (a -> Bool) -> [a]
conditionalRemove [] _ = []
conditionalRemove (x:xs) f | f x = conditionalRemove xs f
                           | otherwise = x : conditionalRemove xs f

-- |This function splits a list using a given function.
-- The third and fourth parameters should be empty lists.
_splitOn :: [b] -> (b -> Bool) -> [b] -> [b] -> [[b]]
_splitOn [] shouldSeperate s constructor = [s]
_splitOn (x:xs) shouldSeperate s constructor | shouldSeperate x = s : _splitOn xs shouldSeperate constructor constructor
                                             | otherwise = _splitOn xs shouldSeperate (s++[x]) constructor

-- |Wrapper around _splitOn for strings.
splitString :: String -> Char -> [String]
splitString s c = _splitOn s (== c) "" ""

-- |Check if a character is invalid.
-- This is the case when it is either a comparison operator (<=>) a mathematical operator (+-*/) a digit (0..9) an assignment operator (~) or a comment identifier (#).
isIllegalVariableCharacter :: Char -> Bool
isIllegalVariableCharacter '<' = True
isIllegalVariableCharacter '>' = True
isIllegalVariableCharacter '=' = True
isIllegalVariableCharacter '?' = True
isIllegalVariableCharacter '~' = True
isIllegalVariableCharacter '#' = True
isIllegalVariableCharacter c | isMathmaticalOperation c = True
                             | isDigit c = True
                             | otherwise = False




-- |Check if a character is a mathematical operator (+-*/).
isMathmaticalOperation :: Char -> Bool
isMathmaticalOperation '*' = True
isMathmaticalOperation '/' = True
isMathmaticalOperation '+' = True
isMathmaticalOperation '-' = True
isMathmaticalOperation _ = False

-- |Return the mathematical priority, or -1 if the character isn't a mathematical operation
getMathematicalPriority :: Char -> Int
getMathematicalPriority '*' = 2
getMathematicalPriority '/' = 2
getMathematicalPriority '+' = 1
getMathematicalPriority '-' = 1
getMathematicalPriority _ = -1

-- |Get first element in tuple.
fst3 :: (a,b,c) -> a
fst3 (x,_,_) = x

-- |Get second element in tuple.
snd3 :: (a,b,c) -> b
snd3 (_,x,_) = x

-- |Get third element in tuple.
trd3 :: (a,b,c) -> c
trd3 (_,_,x) = x

-- |Convert bool too int (True = 1, False = 0).
boolToInt :: Bool -> Int
boolToInt True = 1
boolToInt False = 0

-- |Get first element from list or return error
getFirstArg :: [a] -> MaybeError a
getFirstArg [] = Error "no input file given"
getFirstArg s = NotError $ head s

-- |Check if the last element of a list of strings is "debug"
lastArgIsDebug :: [String] -> Bool
lastArgIsDebug [] = False
lastArgIsDebug ["debug"] = True
lastArgIsDebug (_:xs) = lastArgIsDebug xs