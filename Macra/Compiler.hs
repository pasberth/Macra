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

data CompileError = CompileError deriving (Eq, Show)
data ExpandError = ExpandError deriving (Eq, Show)

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
signatureDefine ((EvalCxtTLNode x):xs) = signatureDefine xs
signatureDefine ((MacCxtTLNode x):xs) =  signatureDefineMacCxtNodes (signatureDefine xs) x
signatureDefine [] = emptySignatureMap

signatureDefineMacCxtNodes :: SignatureMap -> [MacCxtNode] -> SignatureMap
signatureDefineMacCxtNodes sm (node:nodes) = signatureDefineMacCxtNodes
                                           (signatureDefineMacCxtNode sm node)
                                           nodes
signatureDefineMacCxtNodes sm [] = sm

signatureDefineMacCxtNode :: SignatureMap -> MacCxtNode -> SignatureMap
signatureDefineMacCxtNode sm (SigDefMNode id sig) = M.insert id sig sm
signatureDefineMacCxtNode sm _ = sm

macroExpand :: MacroMap -> SignatureMap -> Node -> Either ExpandError Node
macroExpand mm sm node =
  case S.evalState (macroExpand' node) (MacroExpander mm sm [toplevelContext]) of
    ([], node) -> Right node
    (params, node) -> Left ExpandError

macroExpand' :: Node -> MacroExpanderCmd
macroExpand' (SymNode sym) = do
             expander@(MacroExpander mm sm sig) <- S.get
             case M.lookup (cxtId sig, sym) mm of
                  Just macro -> return macro
                  Nothing -> return ([], (SymNode sym))
             where cxtId sig = case sig of
                                    [] -> toplevelContext
                                    (cxtId:sig) -> cxtId


macroExpand' (MaccallNode node arg) = do
             (MacroExpander mm sm sig) <- S.get
             r <- macroExpand' node
             case r of
               ((param:params), curriedMacro) ->
                 return (params, (macroReplace param
                                               curriedMacro
                                               arg))
               ([], SymNode sym) -> do
                 case macroArgExpand mm sm sig' arg of
                   Right arg -> return ([], FuncallNode (SymNode sym) arg)
                   Left err -> fail "missing to apply"
                 where sig' = case M.lookup sym sm of
                                   Nothing -> [toplevelContext]
                                   Just sig' -> sig'
               ([], FuncallNode fn fnarg) -> do
                 case macroArgExpand mm sm [toplevelContext] fn of
                   Right fn ->
                     case macroArgExpand mm sm (tail sig) fnarg of
                       Right fnarg -> return ([], FuncallNode
                                                    (FuncallNode fn fnarg)
                                                    arg)
                       Left err -> fail "missing to apply"
                   Left err -> fail "missing to apply"
               ([], (LambdaNode param body)) -> do
                 case macroArgExpand mm sm [toplevelContext] body of
                   Right arg -> return ([], LambdaNode param arg)
                   l@(Left err) -> fail "missing to apply"
               ([], (DefineNode id expr)) -> do
                 case macroArgExpand mm sm [toplevelContext] node of
                   Right arg -> return ([], DefineNode id arg)
                   l@(Left err) -> fail "missing to apply"
               ([], (PrintNode node)) -> do
                 case macroArgExpand mm sm [toplevelContext] node of
                   Right arg -> return ([], PrintNode arg)
                   l@(Left err) -> fail "missing to apply"
macroExpand' (LambdaNode param body) = do
             -- TODO: this is buggy.
             ([], body) <- macroExpand' body
             return ([], LambdaNode param body)
macroExpand' (DefineNode id expr) = do
             ([], expr) <- macroExpand' expr
             return ([], DefineNode id expr)
macroExpand' (PrintNode node) = do
             ([], node) <- macroExpand' node
             return ([], PrintNode node)
macroExpand' node = return ([], node)

macroArgExpand :: MacroMap -> SignatureMap -> Signature -> Node -> Either ExpandError Node
macroArgExpand mm sm sig node =
    case S.evalState (macroExpand' node) (MacroExpander mm sm sig) of
      ([], node) -> Right node
      (params, node) -> Left $ ExpandError
      --concat [ "macro applying missing: "
      --                                , show params
      --                                , "was not applied."
      --                                ]


macroReplace :: P.Identifier -> Node -> Node -> Node
macroReplace param (SymNode sym) node
             | param == sym = node
             | otherwise = SymNode sym
macroReplace param (MaccallNode a b) node =
             MaccallNode (macroReplace param a node)
                         (macroReplace param b node)
macroReplace param node _ = node

compile :: MacroMap -> SignatureMap -> [ToplevelNode] -> Either CompileError Inst 
compile mm sm ((MacCxtTLNode x):xs) = compile mm sm xs
compile mm sm ((EvalCxtTLNode x):xs) =
        case  (macroExpand mm sm x) of
          Right node ->
            case (compile mm sm xs) of
              Right insts -> Right $ compileNode node insts
              l@(Left err) -> l
          Left err -> Left $ CompileError
compile mm sm [] = Right HaltInst


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