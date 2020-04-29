module Stack where
import Tools
import Data.Char
import Data.Maybe
import Data.List

type Operator       = Char
data FlowNode       = ConditionNode StackNode StackNode | AssignmentNode StackNode StackNode | StartNode [FlowNode] deriving (Show)
data StackNode      = ValueNode Int | UndefinedNode deriving (Show)
data ExpressionNode = FunctionNode FlowNode | IntNode Int | OperationNode Operator StackNode StackNode | StackVariableNode Char deriving (Show)

executeFlowNode :: FlowNode -> [(Char, StackNode)] -> (MaybeError Int, [(Char, StackNode)])
executeFlowNode (StartNode [x]) s = executeFlowNode x s
executeFlowNode (StartNode (x:xs)) s = executeFlowNode x s
executeFlowNode (AssignmentNode (StackVariable node1) node2) s = do
                                                                    let node2Result = executeStackNode node2 s
                                                                    if isError $ fst node2Result
                                                                        then (fst node2Result, s)
                                                                        else do
                                                                            let result = setStack node1 (IntNode $ getValue $ fst node2Result) (snd node2Result)
                                                                            if isError result
                                                                                then (Error $ getError result, s)
                                                                                else(fst node2Result, getValue result)

executeFlowNode _ s = (Error "internal error during executeFlowNode", s)

executeStackNode :: StackNode -> [(Char, StackNode)] -> (MaybeError Int, [(Char, StackNode)])
executeStackNode (FunctionNode f) s = executeFlowNode f s
executeStackNode (ValueNode i) s = (NotError i, s)
executeStackNode (UndefinedNode) s = (Error "stacknode was undefined", s)

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
getFromStack :: Char -> [(Char, StackNode)] -> MaybeError IntNode
getFromStack c stack = if isIllegalVariableCharacter c
                        then Error $ "invalid character lookup: " ++ [c] ++ " only lower case letters are allowed"
                        else do
                             let lookedUp = fromJust $ lookup c stack
                             if isUndefined lookedUp
                                 then Error $ "variable is undefined: " ++ [c]
                                 else NotError lookedUp


setStack ::  Char -> StackNode -> [(Char, StackNode)] -> MaybeError [(Char, StackNode)]
setStack c (StackVariable node) stack = Error "tried to set a variable to a character"
setStack c node stack = if isIllegalVariableCharacter c
                         then Error $ "invalid character set: " ++ [c] ++ " only lower case letters are allowed"
                         else NotError $ map3 _stackSet c node stack


_stackSet :: (Char, StackNode) -> Char -> StackNode -> (Char, StackNode)
_stackSet i c node = if fst i == c
                        then (c, node)
                        else i

prettyStrStack :: [(Char, StackNode)] -> String
prettyStrStack [] = []
prettyStrStack (x:xs) = if isUndefined $ snd x
                            then ""
                            else [fst x] ++ ": " ++ (show $ snd x) ++ ['\n'] ++ (prettyStrStack xs)

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