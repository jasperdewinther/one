-- |This module contains functions for the construction of abstract syntax tree's.
-- The abstract syntax tree is defined as a lookup table with characters as keys and StackNode's (defined in the Stack module) as functions.
module AST where
import Tools
import Data.Char
import Data.Maybe
import Data.List
import Stack
import Text.Read

-- |Get all lines until a new function definition (?) is found.
getOperationsTillFunction :: [String] -> [String]
getOperationsTillFunction [] = []
getOperationsTillFunction (x:xs) | head x == '?' = []
                                 | otherwise = x : getOperationsTillFunction xs

-- |Cluster functions into groups with a function name (names are always just one character)
clusterFunctions :: [String] -> [(Char, [String])]
clusterFunctions [] = []
clusterFunctions (x:xs) | head x == '?' = (last x, getOperationsTillFunction xs) : clusterFunctions xs
                        | otherwise = clusterFunctions xs

-- |Creates a FlowNode based on a single string.
-- Depending on the content of the string it will either be a AssignmentNode or a ConditionNode.
createFlowNode :: String -> MaybeError FlowNode
createFlowNode (a:'~':b) | isError (getExpression b) = Error $ "error while creating flownode in line: \n" ++ [a] ++ ['~'] ++ b ++ "\nwith the following error: \n" ++ getError (getExpression b)
                         | isIllegalVariableCharacter a = Error $ "error while creating flownode in line: \n" ++ [a] ++ ['~'] ++ b ++ "\nillegal character found: \n" ++ [a]
                         | otherwise = NotError $ AssignmentNode (StackVariableNode a) (getValue $ getExpression b)
createFlowNode s | '<' `elem` s = createConditionNodeAbstract (head $ splitString s '<') '<' (last (splitString s '<'))
                 | '=' `elem` s = createConditionNodeAbstract (head $ splitString s '=') '=' (last (splitString s '='))
                 | '>' `elem` s = createConditionNodeAbstract (head $ splitString s '>') '>' (last (splitString s '>'))
                 | otherwise = Error $ "invalid command: \n" ++ s ++ "\nnote that all variables and functions can only consist of one character"

-- |Parses two strings to ExpressionNode and create a ConditionNode.
createConditionNodeAbstract :: String -> Char -> String -> MaybeError FlowNode
createConditionNodeAbstract a char b | isError $ getExpression a = Error $ "error while creating flownode in line: \n" ++ a ++ [char] ++ b ++ "\nwith the following error: \n" ++ getError (getExpression a)
                                     | isError $ getExpression b = Error $ "error while creating flownode in line: \n" ++ a ++ [char] ++ b ++ "\nwith the following error: \n" ++ getError (getExpression b)
                                     |otherwise = NotError $ ConditionNode (getValue $ getExpression a) char (getValue $ getExpression b)

-- |Read a string, check what kind of expression node should be used and create the expression node.
getExpression :: String -> MaybeError ExpressionNode
getExpression s | isNothing (findIndex isMathmaticalOperation s) && allNumbers s = getExpressionNumbers s
                | isNothing $ findIndex isMathmaticalOperation s = getExpressionVariable s
                | any (\x -> getMathematicalPriority x == 1) s = getExpressionWithMath s 1
                | any (\x -> getMathematicalPriority x == 2) s = getExpressionWithMath s 2

-- |Maybe get an expression node when you are sure a string only contains numbers
getExpressionNumbers :: String -> MaybeError ExpressionNode
getExpressionNumbers s | isJust number = NotError $ IntNode $ fromJust number
                       | otherwise = Error $ "number could not be parsed, might be too long: \n" ++ s
                        where number = readMaybe s

-- |Maybe get an expression node when you are sure a string only contains a character that can be used as a variable
getExpressionVariable :: String -> MaybeError ExpressionNode
getExpressionVariable s | (length s == 1) && not (isIllegalVariableCharacter $ head s) = NotError $ StackVariableNode $ head s
                        | isIllegalVariableCharacter $ head s = Error $ "invalid expression, an invalid variable was found: \n" ++ s
                        | otherwise = Error $ "invalid expression: \n" ++ s ++ "\nnote that all variables can only be defined as one character"

-- |Maybe get an expression node when you are sure a string contains a mathematical symbol
getExpressionWithMath :: String -> Int -> MaybeError ExpressionNode
getExpressionWithMath s mathematicalPriority | isMathmaticalOperation operation = let subExpression1 = getExpression headStr
                                                                                      subExpression2 = getExpression $ snd result
                                                                                  in validateExpressions subExpression1 operation subExpression2
                                             | otherwise = Error $ "undefined operation:\n" ++ s
                         where index = findIndex (\x -> getMathematicalPriority x == mathematicalPriority) s
                               result = splitAt (1+fromJust index) s
                               headStr = init (fst result)
                               operation = last (fst result)

-- |Check if two expressions are valid and combine them with an operator
validateExpressions :: MaybeError ExpressionNode -> Operator -> MaybeError ExpressionNode -> MaybeError ExpressionNode
validateExpressions expr1 operation expr2 | isValue expr1 && isValue expr2 = NotError $ OperationNode (getValue expr1) operation (getValue expr2)
                                          | isValue expr1 = Error $ getError expr2
                                          | isValue expr2 = Error $ getError expr1
                                          | otherwise = Error $ getError expr1 ++ getError expr2

-- |Check if a list only contains digits.
allNumbers :: String -> Bool
allNumbers = foldr ((&&) . isDigit) True

-- |Converts a given list with characters and lists of strings to a stack.
createFunctionNodes :: [(Char, [String])] -> MaybeError [(Char, StackNode)]
createFunctionNodes [] = NotError []
createFunctionNodes (x:xs) | isError result = Error $ "Error while creating function node: " ++ [fst x] ++ "\n" ++ getError result
                           | isError otherResult = otherResult
                           | otherwise = NotError $ (fst x, getValue result) : getValue otherResult
                            where result = createFunctionAST (snd x )
                                  otherResult = createFunctionNodes xs

-- |Wrapper around _createFunctionAST to convert [FlowNode] to StackNode.
createFunctionAST :: [String] -> MaybeError StackNode
createFunctionAST s | isError result = Error $ getError result
                    | otherwise = NotError $ StartNode $ getValue result
                     where result = _createFunctionAST s

-- |Create an AST(abstract syntax tree) for a list of strings (a function).
_createFunctionAST :: [String] -> MaybeError [FlowNode]
_createFunctionAST [] = NotError []
_createFunctionAST (x:xs) | isError result = Error $ getError result
                          | isError otherResult = otherResult
                          | otherwise = NotError $ getValue result : getValue otherResult
                           where result = createFlowNode x
                                 otherResult = _createFunctionAST xs

-- |Create AST's for all functions and return stack containing these functions.
createAST :: [String] -> MaybeError [(Char, StackNode)]
createAST s = createFunctionNodes $ clusterFunctions s