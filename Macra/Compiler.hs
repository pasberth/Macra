module Macra.Compiler (compile, initialDefiner, macroDefine) where

import qualified Data.Map as M
import qualified Control.Monad.State as S
import Macra.Parser hiding (Identifier)
import Macra.VM hiding (Identifier)
import qualified Macra.Parser as P
import qualified Macra.VM as VM

data Definer = Definer {
     definerMacroMap :: MacroMap
   , definerContext :: Context
     }

type MacroMap = M.Map (Context, Identifier) Macro
type Context = String
type Identifier = String
type Macro = (MacParams, Node)
type DefinerCmd = S.State Definer MacroMap

{-lambdanode example
  input   : !funcall !lambda foo !add foo 2 3
  parseed : (FuncallNode (LambdaNode (SymNode 'foo') (AddNode (SymNode 'foo') (NumNode 2))) (NumNode 3)) 
  compiled: (FrameInst HaltInst (ConstExpr 1 (ArgInst (CloseInst foo (AddInst (ReferInst 'foo' ReturnInst) (ConstExpr 2 ReturnInst) ReturnInst) ApplyInst))))
-}

initialDefiner :: Definer
initialDefiner = Definer initialMacroMap initialContext

initialContext :: Context
initialContext = "toplevel"

initialMacroMap :: MacroMap
initialMacroMap = M.fromList []

macroDefine :: MacCxtNode -> DefinerCmd
macroDefine (CxtDefMNode (P.SymId cxtId) cxtDef) = do
  definer <- S.get
  S.put definer { definerContext = cxtId }
  macroContextDefine cxtDef

macroContextDefine :: CxtDefMNode -> DefinerCmd
macroContextDefine (MacDefMCNode (P.SymId macId) params node) = do
  definer <- S.get
  S.put definer {
        definerMacroMap = M.insert ((definerContext definer), macId)
                                   (params, node)
                                   (definerMacroMap definer)
        }
  newDefiner <- S.get
  return (definerMacroMap newDefiner)

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
compile (LambdaNode (P.SymId param) expr) next = 
  CloseInst (VM.Sym param) (compile expr ReturnInst) next
compile (DefineNode (P.SymId var) val) next =
  DefineInst (VM.Sym var) next
compile (FuncallNode lambda argument) next = 
  FrameInst next (compile argument (ArgInst (compile lambda ApplyInst)))
{-compile (MaccallNode macro argument) next = 
  FrameInst (FuncallNode (ArgInst (compile argument (macroExpand macro))))

macroExpand :: Node -> LambdaNode-}
