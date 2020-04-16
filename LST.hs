module LST where
import Tools

data FlowNode     = ConditionNode StackNode StackNode | ExpressionNode StackNode StackNode | AssignmentNode StackNode StackNode | StartNode [FlowNode] deriving (Show)
data StackNode    = FunctionNode FlowNode | VariableNode Int | UndefinedNode deriving (Show)


_stackSet :: (Char, StackNode) -> Char -> StackNode -> (Char, StackNode)
_stackSet i c node = if fst i == c
                        then (c, node)
                        else i


setStack ::  Char -> StackNode -> [(Char, StackNode)] -> [(Char, StackNode)]
setStack c node stack = map3 _stackSet c node stack


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