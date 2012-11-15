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
                            , []
                            , (MacroNode (SymNode id))) mm

macroExpand :: MacroMap -> P.CxtId -> Node -> Either ExpandError Node
macroExpand mm cxt node =
  case macroExpandCurry mm cxt node of
    Right ([], [], node) -> Right node
    Right macro -> Left $ ExpandArgumentError macro
    Left err -> Left err

macroExpandCurry :: MacroMap -> P.CxtId -> Node -> Either ExpandError Macro

macroExpandCurry mm _ node@(MacroNode _) = Right ([], [], node)

-- シンボルだった場合、 CxtId に関連付けられた
-- マクロを、 (MacSig, MacParams, Node) の形式で返す。
-- macroExpandCurry を呼んだ側で、 MacSig と MacParams 従って Node を 置換する。
macroExpandCurry mm cxtId node@(SymNode macroId) =
  case M.lookup (cxtId, macroId) mm of
    Just macro -> Right macro
    Nothing -> Right ([], [], node)

macroExpandCurry mm cxtId node@(FuncallNode a b) =
  -- macroExpandCurry でカリー化されたマクロを得る
  case macroExpandCurry mm cxtId a of
    Left err -> Left err
    -- TODO: Right ([], param:params, node) -> ..
    -- for example, when
    -- #[ f x y : a -> b = .. ]

    -- もし aが すべての引数が適用済みのマクロであれば、
    -- b はそのまま a の関数の引数とする。
    Right ([], [], fn) ->
      pure (\b -> ([], [], FuncallNode fn b)) <*> macroExpand mm toplevelContext b
    -- もし aがすべての引数が適用済みであるが、
    -- コンテキストが余っているマクロであれば、
    -- b は関数の引数にしつつコンテキストは変える。
    Right (cxt:sig, [], fn) ->
      pure (\b -> (sig, [], FuncallNode fn b)) <*> macroExpand mm cxt b

    -- 再帰的に置換。
    --
    -- 例:
    --   #[ print a : toplevel -> toplevel
    --              = !print a ]
    --   #[ puts a : toplevel -> toplevel
    --             = print a ]
    --   #[ echo a : toplevel -> toplevel
    --             = puts a ]
    --   echo "hello"
    --
    -- echo "hello" はまず puts "hello" に置換される。そのあと
    -- puts "hello" を同じコンテキストでマクロ展開する。
    Right (cxt:[], param:[], (MacroNode macroNode)) ->
      case pure (macroReplace param macroNode) <*> (macroExpand mm cxt b) of
        Right node -> pure (\x -> x) <*> macroExpandCurry mm cxtId node
        Left err -> Left err

    -- もし a が引数が適用されていないマクロであれば、
    -- a に含まれるparam をすべて b に置換する
    Right (cxt:sig, param:params, (MacroNode macroNode)) ->
      pure (\b -> ( sig
                  , params
                  , (MacroNode (macroReplace param
                                             macroNode
                                             b)))) <*> (macroExpand mm cxt b)
macroExpandCurry mm _ node = pure (\node -> ([], [], node)) <*> macroExpandRecur mm node

-- MacroExpandRecur はたとえば
--   !cons a b
-- の a b もマクロ展開する。この a b は常に toplevel 。
macroExpandRecur :: MacroMap -> Node -> Either ExpandError Node
macroExpandRecur mm node@NilNode      = Right node
macroExpandRecur mm node@(CharNode _) = Right node
macroExpandRecur mm node@(NumNode _)  = Right node
macroExpandRecur mm node@(PrintNode expr) =
  pure (\expr -> PrintNode expr) <*> macroExpand mm toplevelContext expr
macroExpandRecur mm (ConsNode a b) =
  pure (\a b -> ConsNode a b) <*> (macroExpand mm toplevelContext a)
                                        <*> (macroExpand mm toplevelContext b)
macroExpandRecur mm (CarNode a) =
  pure (\a -> CarNode a) <*> (macroExpand mm toplevelContext a)
macroExpandRecur mm (CdrNode a) =
  pure (\a -> CdrNode a) <*> (macroExpand mm toplevelContext a)
macroExpandRecur mm (IfNode condExp thenExp elseExp) =
  pure (\a b c -> IfNode a b c) <*> macroExpand mm toplevelContext condExp
                                <*> macroExpand mm toplevelContext thenExp
                                <*> macroExpand mm toplevelContext elseExp
macroExpandRecur mm (DoNode a b) =
  pure (\a b -> DoNode a b) <*> (macroExpand mm toplevelContext a)
                            <*> (macroExpand mm toplevelContext b)
macroExpandRecur mm (LambdaNode param body) =
  pure (\body -> LambdaNode param body) <*> macroExpand mm toplevelContext body

macroExpandRecur mm (DefineNode id expr) =
  pure (\expr -> DefineNode id expr) <*> macroExpand mm toplevelContext expr

macroReplace :: P.Identifier -> Node -> Node -> Node
macroReplace param NilNode arg = NilNode
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
macroReplace param (DoNode a b) arg =
             DoNode (macroReplace param a arg)
                    (macroReplace param b arg)

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
compileNode (DoNode a b) next = compileNode a (compileNode b next)