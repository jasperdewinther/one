-- |This module is used for creating a stack that can bee searched and edited.
module Stack where
import Tools
import Data.Char
import Data.Maybe
import Data.List
import Data.Maybe

-- |Used to define parts of conditions and expressions.
type Operator       = Char
-- |These are the "steps" that will be taken when executing the program.
data FlowNode       = ConditionNode ExpressionNode Operator ExpressionNode | AssignmentNode ExpressionNode ExpressionNode deriving (Show)
-- |These are objects that ar defined on the stack, either a value or a "StartNode", which is a list of flownodes which is a function.
data StackNode      = ValueNode Int | StartNode [FlowNode] deriving (Show)
-- |Expressions are built using expressionNodes. They can either be numbers, variables or functions defined on the stack or subexpressions.
data ExpressionNode = IntNode Int | OperationNode ExpressionNode Operator ExpressionNode | StackVariableNode Char deriving (Show)

-- |Gets the list of FlowNode's.
getFlowNodes :: StackNode -> [FlowNode]
getFlowNodes (StartNode f) = f

-- |Check if function
isFunction :: StackNode -> Bool
isFunction (StartNode _) = True
isFunction _ = False

-- |Get a StackNode from stack. When the stack gets bigger this function becomes slower as simple lists are used, not a binary tree.
getFromStack :: Char -> [(Char, StackNode)] -> MaybeError StackNode
getFromStack c stack = do
                            let lookedUp = lookup c stack
                            if isJust lookedUp
                                 then NotError $ fromJust lookedUp
                                 else Error $ "variable is undefined: " ++ [c]
-- |Set a value on the stack. When the stack gets bigger this function becomes slower as simple lists are used, not a binary tree.
-- |This wraps _stackSet with a check for illegal characters.
setStack ::  Char -> StackNode -> [(Char, StackNode)] -> MaybeError [(Char, StackNode)]
setStack c node stack = if isIllegalVariableCharacter c
                         then Error $ "invalid character set: " ++ [c] ++ " is an illegal character"
                         else NotError $ _stackSet c node stack

-- |Set a value on the stack. When the stack gets bigger this function becomes slower as simple lists are used, not a binary tree.
_stackSet :: Char -> StackNode -> [(Char, StackNode)] -> [(Char, StackNode)]
_stackSet c node [] = [(c, node)]
_stackSet c node (n:t) = if fst n == c
                            then [(c,node)] ++ t
                            else [n] ++ (_stackSet c node t)

-- |Get a string that represents the stack nicely.
prettyStrStack :: [(Char, StackNode)] -> String
prettyStrStack [] = []
prettyStrStack (x:xs) = [fst x] ++ ": " ++ (show $ snd x) ++ ['\n'] ++ (prettyStrStack xs)

-- |Create empty stack
newStack :: [(Char, StackNode)]
newStack = []