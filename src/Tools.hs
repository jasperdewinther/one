module Tools where

import Data.Char


data MaybeError a = NotError a | Error String


isValue :: MaybeError a -> Bool
isValue (NotError _) = True
isValue _ = False

getValue :: MaybeError a -> a
getValue (NotError m) = m

isError :: MaybeError a -> Bool
isError (Error _) = True
isError _ = False

getError :: MaybeError a -> String
getError (Error m) = m

--removes elements from list that return true on given function
conditionalRemove :: [a] -> (a -> Bool) -> [a]
conditionalRemove [] _ = []
conditionalRemove (x:xs) f = if f x
                            then conditionalRemove xs f
                            else x : conditionalRemove xs f

-- a = input
-- b = element to split on
-- a = previous section
-- a = empty a to use as a constructor
-- [a] = splitted list
_splitOn :: [b] -> (b -> Bool) -> [b] -> [b] -> [[b]]
_splitOn [] shoudlSeperate s constructor = [s]
_splitOn (x:xs) shouldSeperate s constructor = if shouldSeperate x
                                    then (s : _splitOn xs shouldSeperate constructor constructor)
                                    else _splitOn xs shouldSeperate (s++[x]) constructor
splitString :: String -> Char -> [String]
splitString s c = _splitOn s (\x -> x == c) "" ""

isQuestionmark :: Char -> Bool
isQuestionmark c = c == '?'

--basically a normal map function but with 2 extra parameters
map3 :: (d -> a -> b -> f) -> a -> b -> [d] -> [f]
map3 f _ _ [] = []
map3 func fir sec (x:xs) = func x fir sec : map3 func fir sec xs

isIllegalVariableCharacter :: Char -> Bool
isIllegalVariableCharacter c = if c == '<' || --compare
                                  c == '>' || --compare
                                  c == '=' || --compare
                                  c == '?' || --function definition
                                  isMathmaticalOperation c ||
                                  isDigit c ||
                                  c == '~'    --assignment
                                  then True
                                  else False
isMathmaticalOperation :: Char -> Bool
isMathmaticalOperation c = if c == '*' ||
                              c == '/' ||
                              c == '+' ||
                              c == '-'
                              then True
                              else False

fst3 :: (a,b,c) -> a
fst3 (x,_,_) = x

snd3 :: (a,b,c) -> b
snd3 (_,x,_) = x

trd3 :: (a,b,c) -> c
trd3 (_,_,x) = x

boolToInt :: Bool -> Int
boolToInt True = 1
boolToInt False = 0

getFirstArg :: [String] -> MaybeError String
getFirstArg s = if length s == 0
                    then Error "no input file given"
                    else NotError $ head s
lastArgIsDebug :: [String] -> Bool
lastArgIsDebug s = if (head $ reverse s) == "debug"
                      then True
                      else False