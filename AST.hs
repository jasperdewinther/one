module AST where
import Tools
import Data.Char
import Data.Maybe
import Data.List
import Stack



--get all lines until a new function definition is found
getOperationsTillFunction :: [String] -> [String]
getOperationsTillFunction [] = []
getOperationsTillFunction (x:xs) = if isQuestionmark (head x)
                                    then []
                                    else [x] ++ getOperationsTillFunction xs

--cluster functions into groups with a function name
clusterFunctions :: [String] -> [(Char, [String])]
clusterFunctions [] = []
clusterFunctions (x:xs) = if isQuestionmark (head x)
                                then [(head $ reverse x, (getOperationsTillFunction xs))] ++ (clusterFunctions xs)
                                else clusterFunctions xs

createFlowNode :: String -> FlowNode
createFlowNode [a,'=',b] = AssignmentNode (VariableNode a) (ConstIntNode (digitToInt b))
createFlowNode [a,'<',b] = ConditionNode (VariableNode a) (ConstIntNode (digitToInt b))

createFunctionNodes :: [(Char, [String])] -> [(Char, StackNode)]
createFunctionNodes [] = []
createFunctionNodes (x:xs) = [(fst x, FunctionNode $ createFunctionAST (snd x ))] ++ createFunctionNodes xs

--create a startnode which contains all operations in a function
createFunctionAST :: [String] -> FlowNode
createFunctionAST s = StartNode $ _createFunctionAST s

_createFunctionAST :: [String] -> [FlowNode]
_createFunctionAST [] = []
_createFunctionAST (x:xs) = [createFlowNode x] ++ (_createFunctionAST xs)

createAST :: [String] -> [(Char, StackNode)]
createAST s = integrateIntoStack newStack $ createFunctionNodes $ clusterFunctions s