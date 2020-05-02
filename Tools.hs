module Tools where

import Data.Char


data MaybeError a = NotError a | Error String


isValue :: MaybeError a -> Bool
isValue (NotError _) = True
isValue _ = False

isError :: MaybeError a -> Bool
isError (Error _) = True
isError _ = False

getError :: MaybeError a -> String
getError (Error m) = m

getValue :: MaybeError a -> a
getValue (NotError m) = m

conditionalRemove :: [a] -> (a -> Bool) -> [a]
conditionalRemove [] _ = []
conditionalRemove (x:xs) f = if f x
                            then conditionalRemove xs f
                            else x : conditionalRemove xs f

-- the second string is the previous characters in the same section
-- cant be made generic because empty strings need to be made
_splitString :: String -> Char -> String -> [String]
_splitString [] seperator s = [s]
_splitString (x:xs) seperator s = if x == seperator
                                    then (s : _splitString xs seperator "")
                                    else _splitString xs seperator (s++[x])
splitString :: String -> Char -> [String]
splitString s c = _splitString s c ""

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