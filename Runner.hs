module Runner where
import Tools
import Stack

run :: [(Char, StackNode)] -> (MaybeError Int, [(Char, StackNode)])
run stack = do
                let a = getFromStack 'a' stack
                if isValue a
                    then _run (getValue a) stack
                    else (Error "function a not found, function a is always the entry point of the program just like the main function in c++", stack)

_run :: StackNode -> [(Char, StackNode)] -> (MaybeError Int, [(Char, StackNode)])
_run (StartNode functionSteps) stack = executeFunction functionSteps True 0 stack
_run _ stack = (Error "function a not found, function a is always the entry point of the program just like the main function in c++", stack)


--the bool argument has to do with skipping statements after conditionals
-- the int is the last used value and can be the return value of the function
executeFunction :: [FlowNode] -> Bool -> Int -> [(Char, StackNode)] -> (MaybeError Int, [(Char, StackNode)])
executeFunction [] _ i stack = (NotError i, stack)
executeFunction (x:xs) True i stack = do
                                        let result = executeFlowNode x stack
                                        if isValue $ snd3 result
                                            then do
                                                let executeNextCommand = fst3 $ result
                                                let returnValue = getValue $ snd3 $ result
                                                let newStack = trd3 $ result
                                                executeFunction xs executeNextCommand returnValue newStack
                                        else (Error $ "error while executing command:\n" ++ (show x) ++ "\n" ++ (getError $ snd3 result), stack)
--skip this flownode if false
executeFunction (x:xs) False i stack = executeFunction xs True i stack

executeFlowNode :: FlowNode -> [(Char,StackNode)] -> (Bool, MaybeError Int, [(Char, StackNode)])
executeFlowNode (AssignmentNode (StackVariableNode node) expr) stack = do
                                                                        let expressionResult = getExpressionResult expr stack
                                                                        if isValue $ fst expressionResult
                                                                            then do
                                                                              let newStack = setStack node (ValueNode (getValue $ fst expressionResult)) (snd expressionResult)
                                                                              if isValue newStack
                                                                                then (True, NotError (getValue $ fst expressionResult), getValue newStack)
                                                                                else (True, Error $ getError newStack, (snd expressionResult))
                                                                            else (True, fst expressionResult, snd expressionResult)
executeFlowNode (ConditionNode expr1 condition expr2) stack =
executeFlowNode _ stack = (True, Error "Internal error", stack)

conditionHelper :: ExpressionNode -> ExpressionNode -> (Int -> Int -> Int) -> [(Char,StackNode)] -> (MaybeError Int, [(Char,StackNode)])
do
                                            let resultLeft = getExpressionResult expr1 stack
                                            let resultRight = getExpressionResult expr2 $ snd resultLeft
                                            if (isValue $ fst resultLeft) && (isValue $ fst resultRight)
                                              then (NotError $ f (getValue $ fst resultLeft) (getValue $ fst resultRight), snd resultRight)
                                              else (Error $ "invalid expression:\n" ++ (show expr1) ++ (show expr2), stack)

getExpressionResult :: ExpressionNode -> [(Char,StackNode)] -> (MaybeError Int, [(Char,StackNode)])
getExpressionResult (IntNode expr) stack = (NotError expr, stack)
getExpressionResult (StackVariableNode expr) stack = do
                                                        let result = getFromStack expr stack
                                                        if isValue result
                                                            then getStackResult (getValue result) stack
                                                            else (Error $ getError result, stack)
getExpressionResult (OperationNode expr1 '+' expr2) stack = operationHelper expr1 expr2 addition stack
getExpressionResult (OperationNode expr1 '-' expr2) stack = operationHelper expr1 expr2 minus stack
getExpressionResult (OperationNode expr1 '*' expr2) stack = operationHelper expr1 expr2 multiply stack
getExpressionResult (OperationNode expr1 '/' expr2) stack = operationHelper expr1 expr2 divide stack
getExpressionResult (OperationNode expr1 a expr2) stack = (Error $ "this operation does not exist: \n" ++ [a], stack)

operationHelper :: ExpressionNode -> ExpressionNode -> (Int -> Int -> Int) -> [(Char,StackNode)] -> (MaybeError Int, [(Char,StackNode)])
operationHelper  expr1 expr2 f stack = do
                                            let resultLeft = getExpressionResult expr1 stack
                                            let resultRight = getExpressionResult expr2 $ snd resultLeft
                                            if (isValue $ fst resultLeft) && (isValue $ fst resultRight)
                                              then (NotError $ f (getValue $ fst resultLeft) (getValue $ fst resultRight), snd resultRight)
                                              else (Error $ "invalid expression:\n" ++ (show expr1) ++ (show expr2), stack)
multiply :: Int -> Int -> Int
multiply left right = left * right
minus :: Int -> Int -> Int
minus left right = left - right
divide :: Int -> Int -> Int
divide left right = left `div` right
addition :: Int -> Int -> Int
addition left right = left + right


getStackResult :: StackNode -> [(Char,StackNode)] -> (MaybeError Int, [(Char,StackNode)])
getStackResult (ValueNode i) stack = (NotError i, stack)
getStackResult (StartNode flowNodes) stack = executeFunction flowNodes True 0 stack