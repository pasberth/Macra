module Macra.Compiler (compile,
                       macroDefine,
                       macroExpand,
                       emptyMacroMap,
                       toplevelContext) where

import qualified Data.Map as M
import qualified Control.Monad.State as S
import Macra.Parser hiding (Identifier)
import Macra.VM hiding (Identifier)
import qualified Macra.Parser as P
import qualified Macra.VM as VM

data MacroDefiner = MacroDefiner {
     macroDefinerMacroMap :: MacroMap
   , macroDefinerContext :: P.CxtId
     }

type MacroMap = M.Map (P.CxtId, P.Identifier) Macro
type Macro = (MacParams, Node)
type MacroDefinerCmd = S.State MacroDefiner MacroMap

data SignatureDefiner = SignatureDefiner {
     signatureDefinerSignatureMap :: SignatureMap
     }

type SignatureMap = M.Map P.Identifier Signature
type Signature = SigList

{-lambdanode example
  input   : !funcall !lambda foo !add foo 2 3
  parseed : (FuncallNode (LambdaNode (SymNode 'foo') (AddNode (SymNode 'foo') (NumNode 2))) (NumNode 3)) 
  compiled: (FrameInst HaltInst (ConstExpr 1 (ArgInst (CloseInst foo (AddInst (ReferInst 'foo' ReturnInst) (ConstExpr 2 ReturnInst) ReturnInst) ApplyInst))))
-}

toplevelContext :: P.CxtId
toplevelContext = "toplevel"

emptyMacroMap :: MacroMap
emptyMacroMap = M.fromList []

macroDefine :: MacroMap -> P.CxtId -> MacCxtNode -> MacroMap
macroDefine mm cxt node = S.evalState (macroDefine' node) (MacroDefiner mm cxt)

macroDefine' :: MacCxtNode -> MacroDefinerCmd
macroDefine' (CxtDefMNode cxtId cxtDef) = do
  definer <- S.get
  S.put definer { macroDefinerContext = cxtId }
  macroContextDefine cxtDef

macroContextDefine :: CxtDefMNode -> MacroDefinerCmd
macroContextDefine (MacDefMCNode macId params node) = do
  definer <- S.get
  S.put definer {
        macroDefinerMacroMap = M.insert ((macroDefinerContext definer), macId)
                                   (params, node)
                                   (macroDefinerMacroMap definer)
        }
  newDefiner <- S.get
  return (macroDefinerMacroMap newDefiner)

macroExpand :: MacroMap -> P.CxtId -> Node -> Node
macroExpand mm cxt (MaccallNode a b) =
  case a of
  SymNode sym -> case M.lookup (cxt, sym) mm of
                         -- Just m -> m
                         Nothing -> (FuncallNode (SymNode sym) (macroExpand mm cxt b))
  _ -> (FuncallNode (macroExpand mm cxt a) (macroExpand mm cxt b))
macroExpand mm cxt node = node

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
