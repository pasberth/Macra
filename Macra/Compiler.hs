module Macra.Compiler (compile) where

import Macra.Parser (Node(..))
import Macra.VM (Inst(..))
import qualified Macra.Parser as P
import qualified Macra.VM as VM

{-lambdanode example
  input   : !funcall !lambda foo !add foo 2 3
  parseed : (FuncallNode (LambdaNode (SymNode 'foo') (AddNode (SymNode 'foo') (NumNode 2))) (NumNode 3)) 
  compiled: (FrameInst HaltInst (ConstExpr 1 (ArgInst (CloseInst foo (AddInst (ReferInst 'foo' ReturnInst) (ConstExpr 2 ReturnInst) ReturnInst) ApplyInst))))
-}

compile :: Node -> Inst -> Inst

compile (SymNode (P.SymId symbol)) next =
  ReferInst (VM.Sym symbol) next
compile (CharNode chr) next =
  ConstExpr (VM.Char chr) next
compile (NumNode num) next =
  ConstExpr (VM.Double num) next
{-compile (ListNode [node]) nil next =
  ConstExpr (List [Char node]) next-}
compile (IfNode condExp thenExp) next =
  TestInst (compile condExp next) (compile thenExp next) next
compile (LambdaNode (SymNode (P.SymId param)) expr) next = 
  CloseInst (VM.Sym param) (compile expr ReturnInst) next
compile (DefineNode (SymNode (P.SymId var)) val) next =
  DefineInst (VM.Sym var) next
compile (FuncallNode lambda argument) next = 
  FrameInst next (compile argument (ArgInst (compile lambda ApplyInst)))
{-compile (MaccallNode macro argument) next = 
  FrameInst (FuncallNode (ArgInst (compile argument (macroExpand macro))))

macroExpand :: Node -> LambdaNode-}

aLambdaNode = (FuncallNode (LambdaNode (SymNode (P.SymId "foo")) (SymNode (P.SymId "foo"))) (NumNode 3))
aFuncallNode = (FuncallNode (SymNode (P.SymId "foo")) (CharNode 'a'))

-- main = print (compile aLambdaNode HaltInst)
