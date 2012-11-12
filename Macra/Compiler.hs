module Macra.Compiler (MacroMap,
                       compile,
                       macroDefine,
                       macroExpand,
                       emptyMacroMap,
                       toplevelContext) where

import qualified Data.Map as M
import qualified Control.Monad.State as S
import Control.Applicative
import Macra.Parser hiding (Identifier)
import Macra.VM hiding (Identifier)
import qualified Macra.Parser as P
import qualified Macra.VM as VM

type MacroMap = M.Map (P.CxtId, P.Identifier) Macro
type Macro = (MacSig, MacParams, Node)
data CompileError = CompileError
                  | CompileExpandError ExpandError
                  deriving (Eq, Show)

data ExpandError = ExpandError
                 | ExpandArgumentError Macro
                 deriving (Eq, Show)

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

macroExpand :: MacroMap -> P.CxtId -> Node -> Either ExpandError Node
macroExpand mm cxt node =
  case macroExpand' mm cxt node of
    Right ([], [], node) -> Right node
    Right macro -> Left $ ExpandArgumentError macro
    Left err -> Left err

macroExpand' :: MacroMap -> P.CxtId -> Node -> Either ExpandError Macro
macroExpand' mm cxtId NilNode = Right ([], [], NilNode)
macroExpand' mm cxtId node@(SymNode macroId) =
  case M.lookup (cxtId, macroId) mm of
    Just macro -> Right macro
    Nothing -> Right ([], [], node)

macroExpand' mm cxtId node@(CharNode _) = Right ([], [], node)
macroExpand' mm cxtId node@(NumNode _) = Right ([], [], node)
macroExpand' mm cxtId node@(PrintNode expr) =
  pure (\expr -> ([], [], PrintNode expr)) <*> macroExpand mm toplevelContext expr
macroExpand' mm cxtId (ConsNode a b) =
  pure (\a b -> ([], [], ConsNode a b)) <*> (macroExpand mm toplevelContext a)
                                        <*> (macroExpand mm toplevelContext b)
macroExpand' mm cxtId (CarNode a) =
  pure (\a -> ([], [], CarNode a)) <*> (macroExpand mm toplevelContext a)
macroExpand' mm cxtId (CdrNode a) =
  pure (\a -> ([], [], CdrNode a)) <*> (macroExpand mm toplevelContext a)
macroExpand' mm cxtId node@(IfNode condExp thenExp elseExp) =
  pure (\a b c -> ([], [], IfNode a b c)) <*> macroExpand mm toplevelContext condExp
                                          <*> macroExpand mm toplevelContext thenExp
                                          <*> macroExpand mm toplevelContext elseExp
macroExpand' mm cxtId node@(LambdaNode param body) =
  pure (\body -> ([], [], LambdaNode param body)) <*> macroExpand mm toplevelContext body

macroExpand' mm cxtId node@(DefineNode id expr) =
  pure (\expr -> ([], [], DefineNode id expr)) <*> macroExpand mm toplevelContext expr
macroExpand' mm cxtId node@(FuncallNode a b) =
  case macroExpand' mm cxtId a of
    Left err -> Left err
    Right ([], [], fn) ->
      pure (\b -> ([], [], FuncallNode fn b)) <*> macroExpand mm toplevelContext b
    Right (cxt:[], param:[], (MacroNode macroNode)) ->
      case pure (macroReplace param macroNode) <*> (macroExpand mm cxt b) of
        Right node -> pure (\x -> x) <*> macroExpand' mm cxtId node
        Left err -> Left err
    Right (cxt:sig, param:params, (MacroNode macroNode)) ->
      pure (\b -> ( sig
                  , params
                  , (MacroNode (macroReplace param
                                             macroNode
                                             b)))) <*> (macroExpand mm cxt b)

macroReplace :: P.Identifier -> Node -> Node -> Node
macroReplace param node@(MacroNode _) arg = node
macroReplace param node@(SymNode sym) arg
             | param == sym = arg
             | otherwise = node
macroReplace param node@(CharNode _) arg = node
macroReplace param node@(NumNode _) arg = node
macroReplace param node@(FuncallNode a b) arg =
             FuncallNode (macroReplace param a arg)
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
macroReplace param node@(ConsNode a b) arg =
             ConsNode (macroReplace param a arg)
                      (macroReplace param b arg)
macroReplace param node@(CarNode a) arg =
             CarNode (macroReplace param a arg)
macroReplace param (CdrNode a) arg =
             CdrNode (macroReplace param a arg)

macroReplaceSym :: P.Identifier -> P.Identifier -> Node -> P.Identifier
macroReplaceSym param var (P.SymNode arg)
                | param == var = arg
                | otherwise = var
macroReplaceSym param var arg = var

compile :: MacroMap -> [ToplevelNode] -> Either CompileError Inst 
compile mm ((MacCxtTLNode x):xs) = compile mm xs
compile mm ((EvalCxtTLNode x):xs) =
        case  (macroExpand mm toplevelContext x) of
          Right node ->
            case (compile mm xs) of
              Right insts -> Right $ compileNode node insts
              l@(Left err) -> l
          Left err -> Left (CompileExpandError err)
compile mm [] = Right HaltInst


compileNode :: Node -> Inst -> Inst

compileNode NilNode next =
  ConstExpr (VM.List []) next
compileNode (SymNode symbol) next =
  ReferInst symbol next
compileNode (CharNode chr) next =
  ConstExpr (VM.Char chr) next
compileNode (NumNode num) next =
  ConstExpr (VM.Double num) next
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
compileNode (PrintNode argument) next =
  compileNode argument $ PrintInst next
compileNode (MacroNode node) next = compileNode node next
compileNode (ConsNode a b) next = compileNode a (ArgInst (compileNode b (ConsInst next)))
compileNode (CarNode node) next = compileNode node (CarInst next)
compileNode (CdrNode node) next = compileNode node (CdrInst next)
