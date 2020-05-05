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
getOperationsTillFunction (x:xs) = if (head x) == '?'
                                    then []
                                    else [x] ++ getOperationsTillFunction xs

-- |Cluster functions into groups with a function name (names are always just one character)
clusterFunctions :: [String] -> [(Char, [String])]
clusterFunctions [] = []
clusterFunctions (x:xs) = if (head x) == '?'
                                then [(head $ reverse x, (getOperationsTillFunction xs))] ++ (clusterFunctions xs)
                                else clusterFunctions xs

-- |Creates a FlowNode based on a single string.
-- Depending on the content of the string it will either be a AssignmentNode or a ConditionNode.
createFlowNode :: String -> MaybeError FlowNode
createFlowNode (a:'~':b) = if isIllegalVariableCharacter a || (isError $ getExpression b)
                            then do
                                if isIllegalVariableCharacter a
                                    then Error $ "error while creating flownode in line: \n" ++ [a] ++ ['~'] ++ b ++ "\nillegal character found: \n" ++ [a]
                                    else Error $ "error while creating flownode in line: \n" ++ [a] ++ ['~'] ++ b ++ "\nwith the following error: \n" ++ (getError $ getExpression b)
                            else NotError $ AssignmentNode (StackVariableNode a) (getValue $ getExpression b)
createFlowNode s | elem '<' s = createConditionNodeAbstract (head $ splitString s '<') '<' (reverse $ head $ reverse $ splitString s '<')
                 | elem '=' s = createConditionNodeAbstract (head $ splitString s '=') '=' (reverse $ head $ reverse $ splitString s '=')
                 | elem '>' s = createConditionNodeAbstract (head $ splitString s '>') '>' (reverse $ head $ reverse $ splitString s '>')
                 | otherwise = Error $ "invalid command: \n" ++ s ++ "\nnote that all variables and functions can only consist of one character"

-- |Parses two strings to ExpressionNode and create a ConditionNode.
createConditionNodeAbstract :: String -> Char -> String -> MaybeError FlowNode
createConditionNodeAbstract a char b = if isError $ getExpression a
                                          then Error $ "error while creating flownode in line: \n" ++ a ++ [char] ++ b ++ "\nwith the following error: \n" ++ (getError $ getExpression a)
                                          else if isError $ getExpression b
                                              then Error $ "error while creating flownode in line: \n" ++ a ++ [char] ++ b ++ "\nwith the following error: \n" ++ (getError $ getExpression b)
                                              else NotError $ ConditionNode (getValue $ getExpression a) char (getValue $ getExpression b)

-- |Very big and ugly recursive function that parses a string into an ExpressionNode.
getExpression :: String -> MaybeError ExpressionNode
getExpression s = do
                        let index = findIndex (\x -> isMathmaticalOperation x) s
                        if isNothing index
                            then if allNumbers s
                                then do
                                    let number = readMaybe s
                                    if isJust number
                                        then NotError $ IntNode $ fromJust number
                                        else Error $ "number could not be parsed, might be too long: \n" ++ s
                                else if length s == 1
                                    then if isIllegalVariableCharacter $ head s
                                        then Error $ "invalid expression: \n" ++ s
                                        else NotError $ StackVariableNode $ head s
                                    else Error $ "invalid expression: \n" ++ s ++ "\nnote that all variables can only be defined as one character"
                            else do
                                let result = splitAt (1+(fromJust $ index)) s
                                let headStr = reverse $ tail $ reverse $ fst result
                                if allNumbers headStr
                                    then do
                                        let number = readMaybe headStr :: Maybe Int
                                        if isJust number
                                            then do
                                                let operation = head $ reverse $ fst result
                                                if isMathmaticalOperation operation
                                                    then do
                                                        let subExpression = getExpression $ snd result
                                                        if isValue subExpression
                                                            then NotError $ OperationNode (IntNode $ fromJust number) operation (getValue subExpression)
                                                            else Error $ getError subExpression
                                                    else Error $ "undefined operation:\n" ++ s
                                            else Error $ "number could not be parsed, might be too long: \n" ++ headStr ++ ":" ++ (fst result)
                                    else if length headStr == 1
                                         then if isIllegalVariableCharacter $ head headStr
                                             then Error $ "invalid expression: \n" ++ headStr
                                             else do
                                                let operation = head $ reverse $ fst result
                                                if isMathmaticalOperation operation
                                                    then do
                                                        let subExpression = getExpression $ snd result
                                                        if isValue subExpression
                                                            then NotError $ OperationNode ( StackVariableNode $ head headStr) operation (getValue subExpression)
                                                            else Error $ getError subExpression
                                                    else Error $ "undefined operation:\n" ++ s
                                        else Error $ "undefined operation:\n" ++ headStr

-- |Check if a list only contains digits.
allNumbers :: String -> Bool
allNumbers [] = True
allNumbers (x:xs) = if isDigit x
                      then allNumbers xs
                      else False

-- |Converts a given list with characters and lists of strings to a stack.
createFunctionNodes :: [(Char, [String])] -> MaybeError [(Char, StackNode)]
createFunctionNodes [] = NotError []
createFunctionNodes (x:xs) = do
                             let result = createFunctionAST (snd x )
                             if isError result
                                then Error $ "Error while creating function node: " ++ [fst x] ++ "\n" ++ (getError result)
                                else do
                                    let otherResult = createFunctionNodes xs
                                    if isError otherResult
                                     then otherResult
                                     else NotError $ [(fst x, getValue result)] ++ (getValue otherResult)

-- |Wrapper around _createFunctionAST to convert [FlowNode] to StackNode.
createFunctionAST :: [String] -> MaybeError StackNode
createFunctionAST s = do
                        let result = _createFunctionAST s
                        if isError result
                            then Error $ getError result
                            else NotError $ StartNode $ getValue result

-- |Create an AST(abstract syntax tree) for a list of strings (a function).
_createFunctionAST :: [String] -> MaybeError [FlowNode]
_createFunctionAST [] = NotError []
_createFunctionAST (x:xs) = do
                             let result = createFlowNode x
                             if isError result
                                then Error $ getError result
                                else do
                                let otherResult = _createFunctionAST xs
                                if isError otherResult
                                 then otherResult
                                 else NotError $ [getValue result] ++ (getValue otherResult)

-- |Create AST's for all functions and return stack containing these functions.
createAST :: [String] -> MaybeError [(Char, StackNode)]
createAST s = createFunctionNodes $ clusterFunctions s