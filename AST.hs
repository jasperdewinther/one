module AST where
import Tools
import Data.Char
import Data.Maybe
import Data.List

data FlowNode     = ConditionNode StackNode StackNode | ExpressionNode StackNode StackNode | AssignmentNode StackNode StackNode | StartNode [FlowNode] deriving (Show)
data StackNode    = FunctionNode Char FlowNode | ConstIntNode Int | VariableNode Char | UndefinedNode deriving (Show)

_stackSet :: (Char, StackNode) -> Char -> StackNode -> (Char, StackNode)
_stackSet i c node = if fst i == c
                        then if isFunction $ snd i
                              then error $ "cant assign value to already existing function"
                              else (c, node)
                        else i

--basically a normal map function but with 2 extra parameters
map3 :: (d -> a -> b -> f) -> a -> b -> [d] -> [f]
map3 f _ _ [] = []
map3 func fir sec (x:xs) = func x fir sec : map3 func fir sec xs

isFunction :: StackNode -> Bool
isFunction (FunctionNode _ _) = True
isFunction _ = False

setStack ::  Char -> StackNode -> [(Char, StackNode)] -> [(Char, StackNode)]
setStack c node stack = if isLower c
                         then map3 _stackSet c node stack
                         else error $ "invalid character set: " ++ [c] ++ " only lower case letters are allowed"

isUndefined :: StackNode -> Bool
isUndefined (UndefinedNode) = True
isUndefined _ = False

getOperationsTillFunction :: [String] -> [String]
getOperationsTillFunction [] = []
getOperationsTillFunction (x:xs) = if isQuestionmark (head x)
                                    then []
                                    else [x] ++ getOperationsTillFunction xs


clusterFunctions :: [String] -> [(Char, [String])]
clusterFunctions [] = []
clusterFunctions (x:xs) = if isQuestionmark (head x)
                                then [(head $ reverse x, (getOperationsTillFunction xs))] ++ (clusterFunctions xs)
                                else clusterFunctions xs

_createFunctionAST :: [String] -> [FlowNode]
_createFunctionAST [] = []
_createFunctionAST (x:xs) = [createFlowNode x] ++ (_createFunctionAST xs)

createFlowNode :: String -> FlowNode
createFlowNode [a,'=',b] = AssignmentNode (VariableNode a) (ConstIntNode (digitToInt b))

createFunctionAST :: [String] -> FlowNode
createFunctionAST s = StartNode $ _createFunctionAST s

_integrateIntoStack :: [(Char, [StackNode])] -> [(Char, [StackNode])] -> [(Char, [StackNode])]
_integrateIntoStack _ [] = []
_integrateIntoStack (original:xs) (new:ys) = if fst original == fst new
                                              then [new] ++ _integrateIntoStack xs ys
                                              else [original] ++ (_integrateIntoStack xs $ [new]++ys)

integrateIntoStack :: [(Char, [StackNode])] -> [(Char, [StackNode])] -> [(Char, [StackNode])]
integrateIntoStack input new = _integrateIntoStack input $ sortStack new

sortStack :: [(Char, [StackNode])] -> [(Char, [StackNode])]
sortStack input = sortBy (\(a,_) (b,_) -> compare a b) input

--toNodes :: [(Char, [String])] -> [(Char, [StackNode])]
--toNodes (x:xs) =

createAST :: [String] -> [(Char, [String])]
createAST s = clusterFunctions s


getFromStack :: Char -> [(Char, StackNode)] -> StackNode
getFromStack c stack = if isLower c
                        then if isUndefined $ fromJust $ lookup c stack
                         then error $ "variable is undefined: " ++ [c]
                         else fromJust $ lookup c stack
                        else error $ "invalid character lookup: " ++ [c] ++ " only lower case letters are allowed"

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