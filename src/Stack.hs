module Stack where
import Tools
import Data.Char
import Data.Maybe
import Data.List
import Data.Maybe

type Operator       = Char
data FlowNode       = ConditionNode ExpressionNode Operator ExpressionNode | AssignmentNode ExpressionNode ExpressionNode deriving (Show)
data StackNode      = ValueNode Int | StartNode [FlowNode] deriving (Show)
data ExpressionNode = IntNode Int | OperationNode ExpressionNode Operator ExpressionNode | StackVariableNode Char deriving (Show)

isFunction :: StackNode -> Bool
isFunction (StartNode _) = True
isFunction _ = False

--sort stack on character
sortStack :: [(Char, StackNode)] -> [(Char, StackNode)]
sortStack input = sortBy (\(a,_) (b,_) -> compare a b) input

--functions to set/get from stack or create a new stack
getFromStack :: Char -> [(Char, StackNode)] -> MaybeError StackNode
getFromStack c stack = do
                            let lookedUp = lookup c stack
                            if isJust lookedUp
                                 then NotError $ fromJust lookedUp
                                 else Error $ "variable is undefined: " ++ [c]

setStack ::  Char -> StackNode -> [(Char, StackNode)] -> MaybeError [(Char, StackNode)]
setStack c node stack = if isIllegalVariableCharacter c
                         then Error $ "invalid character set: " ++ [c] ++ " is an illegal character"
                         else NotError $ _stackSet c node stack


_stackSet :: Char -> StackNode -> [(Char, StackNode)] -> [(Char, StackNode)]
_stackSet c node [] = [(c, node)]
_stackSet c node (n:t) = if fst n == c
                            then [(c,node)] ++ t
                            else [n] ++ (_stackSet c node t)

prettyStrStack :: [(Char, StackNode)] -> String
prettyStrStack [] = []
prettyStrStack (x:xs) = [fst x] ++ ": " ++ (show $ snd x) ++ ['\n'] ++ (prettyStrStack xs)

newStack :: [(Char, StackNode)]
newStack = []