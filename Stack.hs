module Stack where
import Tools
import Data.Char
import Data.Maybe
import Data.List

data FlowNode     = ConditionNode StackNode StackNode | ExpressionNode StackNode StackNode | AssignmentNode StackNode StackNode | StartNode [FlowNode] deriving (Show)
data StackNode    = FunctionNode FlowNode | ConstIntNode Int | VariableNode Char | UndefinedNode deriving (Show)

isFunction :: StackNode -> Bool
isFunction (FunctionNode _) = True
isFunction _ = False

isUndefined :: StackNode -> Bool
isUndefined (UndefinedNode) = True
isUndefined _ = False

--merge the second paramter into the first
integrateIntoStack :: [(Char, StackNode)] -> [(Char, StackNode)] -> [(Char, StackNode)]
integrateIntoStack input new = _integrateIntoStack input $ sortStack new

_integrateIntoStack :: [(Char, StackNode)] -> [(Char, StackNode)] -> [(Char, StackNode)]
_integrateIntoStack a [] = a
_integrateIntoStack (original:xs) (new:ys) = if fst original == fst new
                                              then [new] ++ _integrateIntoStack xs ys
                                              else [original] ++ (_integrateIntoStack xs $ [new]++ys)
--sort stack on character
sortStack :: [(Char, StackNode)] -> [(Char, StackNode)]
sortStack input = sortBy (\(a,_) (b,_) -> compare a b) input

--functions to set/get from stack or create a new stack
getFromStack :: Char -> [(Char, StackNode)] -> StackNode
getFromStack c stack = if isLower c
                        then if isUndefined $ fromJust $ lookup c stack
                         then error $ "variable is undefined: " ++ [c]
                         else fromJust $ lookup c stack
                        else error $ "invalid character lookup: " ++ [c] ++ " only lower case letters are allowed"

setStack ::  Char -> StackNode -> [(Char, StackNode)] -> [(Char, StackNode)]
setStack c node stack = if isLower c
                         then map3 _stackSet c node stack
                         else error $ "invalid character set: " ++ [c] ++ " only lower case letters are allowed"

_stackSet :: (Char, StackNode) -> Char -> StackNode -> (Char, StackNode)
_stackSet i c node = if fst i == c
                        then if isFunction $ snd i
                              then error $ "cant assign value to already existing function"
                              else (c, node)
                        else i

newStack :: [(Char, StackNode)]
newStack = [  ('a', UndefinedNode ),
              ('b', UndefinedNode ),
              ('c', UndefinedNode ),
              ('d', UndefinedNode ),
              ('e', UndefinedNode ),
              ('f', UndefinedNode ),
              ('g', UndefinedNode ),
              ('h', UndefinedNode ),
              ('i', UndefinedNode ),
              ('j', UndefinedNode ),
              ('k', UndefinedNode ),
              ('l', UndefinedNode ),
              ('m', UndefinedNode ),
              ('n', UndefinedNode ),
              ('o', UndefinedNode ),
              ('p', UndefinedNode ),
              ('q', UndefinedNode ),
              ('r', UndefinedNode ),
              ('s', UndefinedNode ),
              ('t', UndefinedNode ),
              ('u', UndefinedNode ),
              ('v', UndefinedNode ),
              ('w', UndefinedNode ),
              ('x', UndefinedNode ),
              ('y', UndefinedNode ),
              ('z', UndefinedNode ) ]