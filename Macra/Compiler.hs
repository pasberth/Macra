module Macra.Compiler where

-- parser.macra --
data Node = SymNode Identifier
          | CharNode Char
          | NumNode  Double
          | ListNode [Node]
          | IfNode Node Node
          | LambdaNode Node Node
          | DefineNode Node Node
          | FuncallNode Node Node
          | MaccallNode Node Node
          | NilNode
          deriving (Show, Eq)
-- /parser.macra --

{-lambdanode example
  input   : !funcall !lambda foo !add foo 2 3
  parseed : (FuncallNode (LambdaNode (SymNode 'foo') (AddNode (SymNode 'foo') (NumNode 2))) (NumNode 3)) 
  compiled: (FrameInst HaltInst (ConstExpr 1 (ArgInst (CloseInst foo (AddInst (ReferInst 'foo' ReturnInst) (ConstExpr 2 ReturnInst) ReturnInst) ApplyInst))))
-}

data Identifier = SymId String 
                | Nil
                deriving (Show, Eq)

data Value = Double Double  
           | Char Char
           | List [Value]
           deriving (Show, Eq)

data Inst = FrameInst  Inst       Inst      --hasnext
          | ConstExpr  Value      Inst      --hasnext
          | ArgInst    Inst                 --hasnext
          | CloseInst  Identifier Inst Inst --hasnext
          | ApplyInst
          | ReferInst  Identifier Inst      --hasnext
          | ReturnInst
          | TestInst   Inst       Inst Inst --hasnext
          | DefineInst Identifier Inst      --hasnext
          | HaltInst
          | PrintInst  Inst                 --hasnext
          deriving (Show, Eq)

compile :: Node -> Node -> Inst -> Inst

compile (SymNode symbol) nil next =
  ReferInst symbol next
compile (CharNode chr) nil next =
  ConstExpr (Char chr) next
compile (NumNode num) nil next =
  ConstExpr (Double num) next
{-compile (ListNode [node]) nil next =
  ConstExpr (List [Char node]) next-}
compile (IfNode condExp thenExp) nil next =
  TestInst (compile condExp NilNode next) (compile thenExp NilNode next) next
compile (LambdaNode (SymNode param) expr) nil next = 
  CloseInst param (compile expr NilNode ReturnInst) next
compile (DefineNode (SymNode var) val) nil next =
  DefineInst var next
compile (FuncallNode lambda argument) nil next = 
  FrameInst next (compile argument NilNode (ArgInst (compile lambda NilNode ApplyInst)))
{-compile (MaccallNode macro argument) next = 
  FrameInst (FuncallNode (ArgInst (compile argument (macroExpand macro))))

macroExpand :: Node -> LambdaNode-}

aLambdaNode = (FuncallNode (LambdaNode (SymNode (SymId "foo")) (SymNode (SymId "foo"))) (NumNode 3))
aFuncallNode = (FuncallNode (SymNode (SymId "foo")) (CharNode 'a'))

main = print (compile aLambdaNode NilNode HaltInst)
