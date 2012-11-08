module Macra.Compiler (MacroMap,
                       compile,
                       macroDefine,
                       signatureDefine,
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

data MacroExpander = MacroExpander {
     macroExpanderMacroMap :: MacroMap
   , macroExpanderSignatureMap :: SignatureMap
   , macroExpanderSignature :: Signature
     }
type MacroExpanderCmd = S.State MacroExpander Macro

{-lambdanode example
  input   : !funcall !lambda foo !add foo 2 3
  parseed : (FuncallNode (LambdaNode (SymNode 'foo') (AddNode (SymNode 'foo') (NumNode 2))) (NumNode 3)) 
  compiled: (FrameInst HaltInst (ConstExpr 1 (ArgInst (CloseInst foo (AddInst (ReferInst 'foo' ReturnInst) (ConstExpr 2 ReturnInst) ReturnInst) ApplyInst))))
-}

toplevelContext :: P.CxtId
toplevelContext = "toplevel"

emptyMacroMap :: MacroMap
emptyMacroMap = M.fromList []

emptySignatureMap :: SignatureMap
emptySignatureMap = M.fromList []

macroDefine :: [ToplevelNode] -> MacroMap
macroDefine ((EvalCxtTLNode x):xs) = macroDefine xs
macroDefine ((MacCxtTLNode x):xs) = macroDefineMacCxtNodes (macroDefine xs) x
macroDefine [] = emptyMacroMap

macroDefineMacCxtNodes :: MacroMap -> [MacCxtNode] -> MacroMap
macroDefineMacCxtNodes mm (node:nodes) = macroDefineMacCxtNodes
                                           (macroDefineMacCxtNode mm node)
                                           nodes
macroDefineMacCxtNodes mm [] = mm

macroDefineMacCxtNode :: MacroMap -> MacCxtNode -> MacroMap
macroDefineMacCxtNode mm node = S.evalState (macroDefineMacCxtNode' node) (MacroDefiner mm toplevelContext)

macroDefineMacCxtNode' :: MacCxtNode -> MacroDefinerCmd
macroDefineMacCxtNode' (CxtDefMNode cxtId (cxtDefs)) = do
  definer <- S.get
  S.put definer { macroDefinerContext = cxtId }
  macroContextDefine cxtDefs
macroDefineMacCxtNode' _ = return emptyMacroMap

macroContextDefine :: [CxtDefMNode] -> MacroDefinerCmd
macroContextDefine (x:xs) = macroContextDefine' x >> macroContextDefine xs
macroContextDefine [] = do
                      definer <- S.get
                      return $ macroDefinerMacroMap definer

macroContextDefine' :: CxtDefMNode -> MacroDefinerCmd
macroContextDefine' (MacDefMCNode macId params node) = do
  definer <- S.get
  S.put definer {
        macroDefinerMacroMap = M.insert ((macroDefinerContext definer), macId)
                                   (params, node)
                                   (macroDefinerMacroMap definer)
        }
  newDefiner <- S.get
  return (macroDefinerMacroMap newDefiner)

signatureDefine :: [ToplevelNode] -> SignatureMap
signatureDefine nodes = emptySignatureMap

macroExpand :: MacroMap -> SignatureMap -> Node -> Node
macroExpand mm sm node =
  case S.evalState (macroExpand' node) (MacroExpander mm sm [toplevelContext]) of
    ([], node) -> node
    (params, node) -> node -- missing to apply

macroExpand' :: Node -> MacroExpanderCmd
macroExpand' (SymNode sym) = do
             (MacroExpander mm sm (cxtId:sig)) <- S.get
             case M.lookup (cxtId, sym) mm of
                  Just macro -> return macro
                  Nothing -> return ([], (SymNode sym))
macroExpand' (MaccallNode node arg) = do
             (MacroExpander mm sm sig) <- S.get
             r <- macroExpand' node
             case r of
               ((param:params), unreplacedNodeA) ->
                 return (params, (macroReplace param
                                               unreplacedNodeA
                                               arg))
               ([], (SymNode sym)) -> do
                 case M.lookup sym sm of
                   Just sig' -> do
                     S.put (MacroExpander mm sm sig')
                     r' <- macroExpand' arg
                     S.put (MacroExpander mm sm sig)
                     case r' of
                       ([], arg) -> return ([], FuncallNode node arg)
                       (_, arg) -> fail "missing to apply"
macroExpand' node = return ([], node)

macroReplace :: P.Identifier -> Node -> Node -> Node
macroReplace param (SymNode sym) node
             | param == sym = node
             | otherwise = SymNode sym
macroReplace param (MaccallNode a b) node =
             MaccallNode (macroReplace param a node)
                         (macroReplace param b node)
macroReplace param node _ = node

compile :: MacroMap -> SignatureMap -> [ToplevelNode] -> Inst 
compile mm sm ((MacCxtTLNode x):xs) = compile mm sm xs
compile mm sm ((EvalCxtTLNode x):xs) = compileNode (macroExpand mm sm x) (compile mm sm xs)
compile mm sm [] = HaltInst


compileNode :: Node -> Inst -> Inst

compileNode (SymNode (P.SymId symbol)) next =
  ReferInst (VM.Sym symbol) next
compileNode (CharNode chr) next =
  ConstExpr (VM.Char chr) next
compileNode (NumNode num) next =
  ConstExpr (VM.Double num) next
{-compile (ListNode [node]) nil next =
  ConstExpr (List [Char node]) next-}
compileNode (IfNode condExp thenExp) next =
  TestInst (compileNode condExp next) (compileNode thenExp next) next
compileNode (LambdaNode (P.SymId param) expr) next = 
  CloseInst (VM.Sym param) (compileNode expr ReturnInst) next
compileNode (DefineNode (P.SymId var) val) next =
  compileNode val $ DefineInst (VM.Sym var) next
compileNode (FuncallNode lambda argument) next = 
  FrameInst next (compileNode argument (ArgInst (compileNode lambda ApplyInst)))
compileNode (MaccallNode a b) next =
  compileNode (FuncallNode a b) next
compileNode (PrintNode argument) next =
  compileNode argument $ PrintInst next