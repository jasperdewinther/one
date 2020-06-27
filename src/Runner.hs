-- |This module is used for running an abstract syntax tree (defined in the AST module).
module Runner where
import Tools
import Stack

-- |Run a given stack starting at function a.
-- Return the return value and the resulting stack.
-- The return value can be an error, then the stack can be used alongside with the error for debugging.
run :: [(Char, StackNode)] -> (MaybeError Int, [(Char, StackNode)])
run stack | isValue $ getFromStack 'a' stack = executeFunction (getFlowNodes $ getValue $ getFromStack 'a' stack) True 0 stack
          | otherwise = (Error "function a not found, function a is always the entry point of the program just like the main function in c++", stack)

-- |Execute a list of FlowNode's.
-- If the bool argument is False the next statement will be ignored. This happens after conditionals.
-- The int is the last used value and can be the return value of the function.
-- And the last argument is the stack.
-- The return tuple contains the result and the resulting stack.
executeFunction :: [FlowNode] -> Bool -> Int -> [(Char, StackNode)] -> (MaybeError Int, [(Char, StackNode)])
executeFunction [] _ i stack = (NotError i, stack)
executeFunction (x:xs) True i stack = do
                                        let result = executeFlowNode x stack
                                        if isValue $ snd3 result
                                            then do
                                                let executeNextCommand = fst3 result
                                                let returnValue = getValue $ snd3 result
                                                let newStack = trd3 result
                                                executeFunction xs executeNextCommand returnValue newStack
                                        else (Error $ "error while executing command:\n" ++ show x ++ "\n" ++ getError (snd3 result), stack)
-- Skip this flownode if false
executeFunction (x:xs) False i stack = executeFunction xs True i stack

-- |Execute a single FlowNode.
-- |Return whether the next statement must be skipped, the resulting int and the resulting stack.
executeFlowNode :: FlowNode -> [(Char,StackNode)] -> (Bool, MaybeError Int, [(Char, StackNode)])
executeFlowNode (AssignmentNode (StackVariableNode node) expr) stack | isValue $ fst expressionResult = 
                                                                              let newStack = setStack node (ValueNode (getValue $ fst expressionResult)) (snd expressionResult)
                                                                              in failOrSucceedFlownode newStack (getValue $ fst expressionResult) stack
                                                                     | otherwise = (True, fst expressionResult, snd expressionResult)
                                                                     where expressionResult = getExpressionResult expr stack
executeFlowNode (ConditionNode expr1 '<' expr2) stack = conditionHelper expr1 expr2 (<) stack
executeFlowNode (ConditionNode expr1 '>' expr2) stack = conditionHelper expr1 expr2 (>) stack
executeFlowNode (ConditionNode expr1 '=' expr2) stack = conditionHelper expr1 expr2 (==) stack
executeFlowNode _ stack = (True, Error "Internal error", stack)

-- |Validate a new stack, throw error if the stack is not valid and use fallbackstack to keep the last stack state before crashing
failOrSucceedFlownode :: MaybeError [(Char,StackNode)] -> Int -> [(Char,StackNode)] -> (Bool, MaybeError Int, [(Char, StackNode)])
failOrSucceedFlownode stack returnValue fallbackStack | isValue stack = (True, NotError returnValue, getValue stack)
                                                      | otherwise = (True, Error "Could not set variable in stack", fallbackStack)

-- |Uses two expressions a function and a stack to test a condition and return the result.
conditionHelper :: ExpressionNode -> ExpressionNode -> (Int -> Int -> Bool) -> [(Char,StackNode)] -> (Bool, MaybeError Int, [(Char,StackNode)])
conditionHelper expr1 expr2 f stack | validExpressions expr1 expr2 stack = let result1 = getExpressionResult expr1 stack
                                                                               result2 = getExpressionResult expr2 $ snd result1
                                                                           in (f (getValue $ fst result1) (getValue $ fst result2), NotError $ boolToInt $ f (getValue $ fst result1) (getValue $ fst result2), snd result2)
                                    | otherwise = (True, Error $ "invalid expression:\n" ++ show expr1 ++ show expr2, stack)

-- |Gets the result of an ExpressionNode, this is dependent on the given stack.
getExpressionResult :: ExpressionNode -> [(Char,StackNode)] -> (MaybeError Int, [(Char,StackNode)])
getExpressionResult (IntNode expr) stack = (NotError expr, stack)
getExpressionResult (StackVariableNode expr) stack | isInStack expr stack  = getStackResult (getValue $ getFromStack expr stack) stack
                                                   | otherwise = (Error $ getError $ getFromStack expr stack, stack)
getExpressionResult (OperationNode expr1 '+' expr2) stack = operationHelper expr1 expr2 (+) stack
getExpressionResult (OperationNode expr1 '-' expr2) stack = operationHelper expr1 expr2 (-) stack
getExpressionResult (OperationNode expr1 '*' expr2) stack = operationHelper expr1 expr2 (*) stack
getExpressionResult (OperationNode expr1 '/' expr2) stack = operationHelper expr1 expr2 divide stack
getExpressionResult (OperationNode expr1 a expr2) stack = (Error $ "this operation does not exist: \n" ++ [a], stack)

-- |An abstraction layer for handling mathematical operations.
operationHelper :: ExpressionNode -> ExpressionNode -> (Int -> Int -> Int) -> [(Char,StackNode)] -> (MaybeError Int, [(Char,StackNode)])
operationHelper expr1 expr2 f stack | validExpressions expr1 expr2 stack = getOperationResult expr1 expr2 f stack
                                    | otherwise = (Error $ "invalid expression:\n" ++ show expr1  ++ " " ++ show expr2, stack)

-- |Check if two expressions are valid when executing them after each other
validExpressions :: ExpressionNode -> ExpressionNode -> [(Char,StackNode)] -> Bool
validExpressions expr1 expr2 stack = isValue (fst $ getExpressionResult expr1 stack) && isValue (fst $ getExpressionResult expr2 newStack)
                                    where newStack = snd $ getExpressionResult expr1 stack

-- |Get the result of two expressions with a given function.
-- |Make sure the expressions are valid, this can be done with the function validExpressions.
getOperationResult :: ExpressionNode -> ExpressionNode -> (Int -> Int -> Int) -> [(Char,StackNode)] -> (MaybeError Int, [(Char,StackNode)])
getOperationResult expr1 expr2 f stack = let result1 = getExpressionResult expr1 stack
                                             result2 = getExpressionResult expr2 $ snd result1
                                         in (NotError $ f (getValue $ fst result1) (getValue $ fst result2), snd result2)

-- |Integer division.
divide :: Int -> Int -> Int
divide left right = left `div` right

-- |Gets a value from the stack.
-- This can either be a simple integer or the result of a function.
getStackResult :: StackNode -> [(Char,StackNode)] -> (MaybeError Int, [(Char,StackNode)])
getStackResult (ValueNode i) stack = (NotError i, stack)
getStackResult (StartNode flowNodes) stack = executeFunction flowNodes True 0 stack