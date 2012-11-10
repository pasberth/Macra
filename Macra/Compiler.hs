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
                            , (MacroNode (recur (SymNode id) params))) mm
  where recur f [] = f
        recur f params = (FuncallNode (recur f (init params))
                                      (SymNode (last params)))

macroExpand :: MacroMap -> Node -> Either ExpandError Node
macroExpand mm node =
  case macroExpand' mm toplevelContext node of
    ([], [], node) -> Right node
    otherwise -> Left ExpandError

macroArgExpand :: MacroMap -> P.CxtId -> Node -> Node
macroArgExpand mm cxtId node =
  case macroExpand' mm cxtId node of
    ([], [], node) -> node
    -- otherwise -> Left ExpandError

macroExpand' :: MacroMap -> P.CxtId -> Node -> Macro
macroExpand' mm cxtId node@(SymNode macroId) =
  case M.lookup (cxtId, macroId) mm of
    Just macro -> macro
    Nothing -> ([], [], node)
macroExpand' mm cxtId node@(MaccallNode a b) =
  case macroExpand' mm cxtId a of
    ([], [], fn) ->
        ( []
        , []
        , FuncallNode fn (macroArgExpand mm toplevelContext b))
    (cxt:sig, param:params, (MacroNode macroNode)) ->
        ( sig
        , params
        , (MacroNode (macroReplace param
                                   macroNode
                                   (macroArgExpand mm cxt b))))
macroExpand' mm cxtId node = ([], [], node)

macroReplace :: P.Identifier -> Node -> Node -> Node
macroReplace param node@(MacroNode _) arg = node
macroReplace param node@(SymNode sym) arg
             | param == sym = arg
             | otherwise = node
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