module Macra.Compiler (MacroMap,
                       compile,
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

type MacroMap = M.Map (P.CxtId, P.Identifier) Macro
type Macro = (MacSig, MacParams, Node)
data CompileError = CompileError deriving (Eq, Show)
data ExpandError = ExpandError deriving (Eq, Show)

toplevelContext :: P.CxtId
toplevelContext = "toplevel"

emptyMacroMap :: MacroMap
emptyMacroMap = M.fromList []

macroDefine :: [ToplevelNode] -> MacroMap
macroDefine ((EvalCxtTLNode x):xs) = macroDefine xs
macroDefine ((MacCxtTLNode x):xs) = macroDefineMacCxtNode (macroDefine xs) x
macroDefine [] = emptyMacroMap

macroDefineMacCxtNode :: MacroMap -> MacCxtNode -> MacroMap
macroDefineMacCxtNode mm (MacDef1MNode id [] params node) =
  M.insert (toplevelContext, id) ([], params, node) mm
macroDefineMacCxtNode mm (MacDef1MNode id sig params node) =
  M.insert ((last sig), id) ((init sig), params, node) mm
macroDefineMacCxtNode mm (MacDef2MNode id sig params) =
  M.insert ((last sig), id) ( (init sig)
                            , params
                            , (recur (SymNode id) params)) mm
  where recur f [] = f
        recur f params = (FuncallNode (recur f (init params))
                                      (SymNode (last params)))
{-
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
macroReplace param (LambdaNode var body) node =
             LambdaNode (macroReplaceSym param var node)
                        (macroReplace param body node)
macroReplace param node _ = node

macroReplaceSym :: P.Identifier -> P.Identifier -> Node -> P.Identifier
macroReplaceSym param id (SymNode sym)
                | param == id = sym
                | otherwise = id
-- m a b = !lambda a b
-- m (f a) b
-- ===> !lambda (f a) b
macroReplaceSym param id _ = id
-}
macroExpand :: MacroMap -> Node -> Either ExpandError Node
macroExpand mm node = Right node
compile :: MacroMap -> [ToplevelNode] -> Either CompileError Inst 
compile mm ((MacCxtTLNode x):xs) = compile mm xs
compile mm ((EvalCxtTLNode x):xs) =
        case  (macroExpand mm x) of
          Right node ->
            case (compile mm xs) of
              Right insts -> Right $ compileNode node insts
              l@(Left err) -> l
          Left err -> Left $ CompileError
compile mm [] = Right HaltInst


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