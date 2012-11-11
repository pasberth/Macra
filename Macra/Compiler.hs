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
macroExpand' mm cxtId node@(PrintNode expr) =
  ([], [], PrintNode $ macroArgExpand mm toplevelContext expr)
macroExpand' mm cxtId node@(IfNode condExp thenExp elseExp) =
  ([], [], IfNode (macroArgExpand mm toplevelContext condExp)
                  (macroArgExpand mm toplevelContext thenExp)
                  (macroArgExpand mm toplevelContext elseExp))
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
macroReplace param node@(CharNode _) arg = node
macroReplace param node@(NumNode _) arg = node
macroReplace param node@(ListNode []) arg = node
macroReplace param node@(ListNode nodes) arg =
             ListNode $ map (flip (macroReplace param) arg) nodes
macroReplace param node@(FuncallNode a b) arg =
             FuncallNode (macroReplace param a arg)
                         (macroReplace param b arg)
macroReplace param node@(MaccallNode a b) arg =
             MaccallNode (macroReplace param a arg)
                         (macroReplace param b arg)
macroReplace param node@(IfNode a b c) arg =
             IfNode (macroReplace param a arg)
                    (macroReplace param b arg)
                    (macroReplace param c arg)
macroReplace param node@(LambdaNode var body) arg =
             LambdaNode (macroReplaceSym param var arg)
                        (macroReplace param body arg)
macroReplace param node@(DefineNode id expr) arg =
             DefineNode (macroReplaceSym param id arg)
                        (macroReplace param expr arg)
macroReplace param node@(PrintNode expr) arg =
             PrintNode (macroReplace param expr arg)

macroReplaceSym :: P.Identifier -> P.Identifier -> Node -> P.Identifier
macroReplaceSym param var (P.SymNode arg)
                | param == var = arg
                | otherwise = var
macroReplaceSym param var arg = var

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

compileNode (SymNode symbol) next =
  ReferInst symbol next
compileNode (CharNode chr) next =
  ConstExpr (VM.Char chr) next
compileNode (NumNode num) next =
  ConstExpr (VM.Double num) next
{-compile (ListNode [node]) nil next =
  ConstExpr (List [Char node]) next-}
compileNode (IfNode condExp thenExp elseExp) next =
  compileNode condExp $
    TestInst (compileNode thenExp next)
             (compileNode elseExp next)
compileNode (LambdaNode param expr) next = 
  CloseInst param (compileNode expr ReturnInst) next
compileNode (DefineNode var val) next =
  compileNode val $ DefineInst var next
compileNode (FuncallNode lambda argument) next = 
  FrameInst next (compileNode argument (ArgInst (compileNode lambda ApplyInst)))
compileNode (MaccallNode a b) next =
  compileNode (FuncallNode a b) next
compileNode (PrintNode argument) next =
  compileNode argument $ PrintInst next
compileNode (MacroNode node) next = compileNode node next