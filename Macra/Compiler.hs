module Macra.Compiler (MacroMap,
                       compile,
                       macroDefine,
                       macroExpand,
                       emptyMacroMap) where

import qualified Data.Map as M
import qualified Control.Monad.State as S
import Control.Applicative
import Macra.Parser hiding (Identifier)
import Macra.VM hiding (Identifier)
import qualified Macra.Parser as P
import qualified Macra.VM as VM

-- コンテキスト & id とマクロの関連。
-- macroDefine で作り、 macroExpand で使用する。
-- たとえば
-- #[ m a : t -> u = a ]
-- であれば CxtId は u で Identifier は m となる。
type MacroMap = M.Map (P.CxtId, P.Identifier) Macro

-- たとえば
-- #[ m a : t -> u = a ]
-- であれば (["t"], ["a"], (SymNode "a"))
-- のような形。 
type Macro = (MacSig, MacParams, Node)

-- compile 関数がコンパイルに失敗したとき返す型
-- TODO: とりあえず用意しただけ。
--       あとでメッセージとかちゃんと書く
data CompileError = CompileError
                  | CompileExpandError ExpandError
                  deriving (Eq, Show)

-- macroExpand 関数がマクロ展開に失敗したとき返す型
-- TODO: とりあえず用意しただけ。
--       あとでメッセージとかちゃんと書く
data ExpandError = ExpandError
                 | ExpandArgumentError Macro [Node]
                 deriving (Eq, Show)

emptyMacroMap :: MacroMap
emptyMacroMap = M.fromList []

macroDefine :: [MacCxtNode] -> MacroMap
macroDefine (x:xs) = macroDefineMacCxtNode (macroDefine xs) x
macroDefine [] = emptyMacroMap

macroDefineMacCxtNode :: MacroMap -> MacCxtNode -> MacroMap
-- macroDefineMacCxtNode toplevelContext mm (MacDef1MNode id [] params node) =
--   M.insert (toplevelContext, id) ([], params, node) mm
macroDefineMacCxtNode mm (MacDef1MNode id sig params node) =
  M.insert ((last sig), id) ((init sig), params, node) mm
macroDefineMacCxtNode mm (MacDef2MNode id sig params) =
  M.insert ((last sig), id) ( (init sig)
                            , []
                            , (SymNode id)) mm
macroDefineMacCxtNode mm (Shebang _ _) = mm

macroExpand :: P.CxtId -> MacroMap -> P.CxtId -> Node -> Either ExpandError Node

macroExpand toplevelContext mm cxt (IfNode a b c) =
  pure IfNode
       <*> macroExpand toplevelContext mm cxt a
       <*> macroExpand toplevelContext mm cxt b
       <*> macroExpand toplevelContext mm cxt c
macroExpand toplevelContext mm cxt (LambdaNode var b) =
  pure (LambdaNode var)
       <*> macroExpand toplevelContext mm cxt b
macroExpand toplevelContext mm cxt (DefineNode var b) =
  pure (DefineNode var)
       <*> macroExpand toplevelContext mm cxt b
macroExpand toplevelContext mm cxt (PrintNode a) =
  pure PrintNode
       <*> macroExpand toplevelContext mm cxt a
macroExpand toplevelContext mm cxt (ConsNode a b) =
  pure ConsNode
       <*> macroExpand toplevelContext mm cxt a
       <*> macroExpand toplevelContext mm cxt b
macroExpand toplevelContext mm cxt (CarNode a) =
  pure PrintNode
       <*> macroExpand toplevelContext mm cxt a
macroExpand toplevelContext mm cxt (CdrNode a) =
  pure PrintNode
       <*> macroExpand toplevelContext mm cxt a
macroExpand toplevelContext mm cxt (DoNode a b) =
  pure DoNode
       <*> macroExpand toplevelContext mm cxt a
       <*> macroExpand toplevelContext mm cxt b
-- SymNode か Funcall かあるいは NilNode や CharNode など引数のない Node の場合
macroExpand toplevelContext mm cxt node =
  case lookupMacro mm cxt node of
    Nothing -> Right node
    Just (macro@(sig, params, macroNode), args)
      | length params /= length args -> Left $ ExpandArgumentError macro args
      -- もし sig が足りなければ、すべて toplevel として扱う
      | otherwise -> (expandArgs sigAndArgs) >>= (\args -> macroReplace macroNode params args)
      where expandArg (cxt, arg) = macroExpand toplevelContext mm cxt arg
            sigAndArgs :: [(CxtId, Node)]
            sigAndArgs = zip (sig ++ repeat toplevelContext) args
            expandArgs :: [(CxtId, Node)] -> Either ExpandError [Node]
            expandArgs [] = Right []
            expandArgs (x:xs) = expandArg x >>= (\x -> expandArgs xs >>= (\xs -> Right (x:xs)))

-- Node からマクロと引数を取り出す。
-- たとえば !funcall !funcall f a b で、もし f マクロが定義されているなら
-- Just (macro, [a, b]) を返す。もしマクロがないなら Nothing を返す
lookupMacro :: MacroMap -> P.CxtId -> Node -> Maybe (Macro, [Node])
lookupMacro mm cxtId node@(SymNode macroId) =
  pure (\macro -> (macro, [])) <*> M.lookup (cxtId, macroId) mm
lookupMacro mm cxtId node@(FuncallNode a b) =
  pure (\(macro, args) -> (macro, args ++ [b])) <*> lookupMacro mm cxtId a
lookupMacro _ _ _ = Nothing

macroReplace :: Node -> MacParams -> [Node] -> Either ExpandError Node
macroReplace node [] [] = Right node
macroReplace node@(SymNode sym) (param:params) (arg:args)
             | param == sym = Right arg
             | otherwise = macroReplace node params args
macroReplace NilNode _ _ = Right NilNode
macroReplace node@(CharNode _) _ _ = Right node
macroReplace node@(NumNode _) _ _ = Right node
macroReplace (FuncallNode a b) params args =
             pure FuncallNode
                  <*> (macroReplace a params args)
                  <*> (macroReplace b params args)
macroReplace (IfNode a b c) params args =
             pure IfNode
                  <*> (macroReplace a params args)
                  <*> (macroReplace b params args)
                  <*> (macroReplace c params args)
macroReplace (LambdaNode var b) params args =
             pure LambdaNode
                  <*> (macroReplaceSym var params args)
                  <*> (macroReplace b params args)
macroReplace (DefineNode var b) params args =
             pure DefineNode
                  <*> (macroReplaceSym var params args)
                  <*> (macroReplace b params args)
macroReplace (PrintNode a) params args =
             pure PrintNode <*> (macroReplace a params args)
macroReplace (ConsNode a b) params args =
             pure ConsNode
                  <*> (macroReplace a params args)
                  <*> (macroReplace b params args)
macroReplace (CarNode a) params args =
             pure CarNode <*> (macroReplace a params args)
macroReplace (CdrNode a) params args =
             pure CdrNode <*> (macroReplace a params args)
macroReplace (DoNode a b) params args =
             pure DoNode <*> (macroReplace a params args)
                         <*> (macroReplace b params args)

macroReplaceSym :: P.Identifier -> [P.Identifier] -> [Node] -> Either ExpandError P.Identifier
macroReplaceSym var [] [] = Right var
macroReplaceSym var (param:params) ((P.SymNode arg):args)
                | param == var = Right arg
                | otherwise = macroReplaceSym var params args

macroReplaceSym var (param:params) (arg:args)
                -- たとえば
                --   #[ a => b : t = !lambda a b  ] と定義して、
                -- (1, 2) => x が !lambda (1, 2) x に展開されてしまった場合など。
                -- TODO: 単に ExpandError ではなく、なにかメッセージを付ける
                | param == var = Left ExpandError
                | otherwise = macroReplaceSym var params args

compile :: P.CxtId -> MacroMap -> Node -> Either CompileError Inst 
compile toplevelContext mm x =
        case  (macroExpand toplevelContext mm toplevelContext x) of
          Right node -> Right (compileNode node HaltInst)
          Left err -> Left (CompileExpandError err)

compileNode :: Node -> Inst -> Inst

compileNode NilNode next =
  ConstExpr (VM.List []) next
compileNode (SymNode symbol) next =
  ReferInst symbol (ThawInst next) --Refer returns a thunk. Thaw extracs the thunk
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
  -- Save call-frame before funcalling. funcall applies `lambda` to `argument`
  -- Freeze the argument to create a thunk. (Lazy evaluation)
  FrameInst next (FreezeInst (compileNode argument HaltInst) (ArgInst (compileNode lambda ApplyInst)))
compileNode (PrintNode argument) next =
  compileNode argument $ PrintInst next
compileNode (ConsNode a b) next = compileNode a (ArgInst (compileNode b (ConsInst next)))
compileNode (CarNode node) next = compileNode node (CarInst next)
compileNode (CdrNode node) next = compileNode node (CdrInst next)
compileNode (DoNode a b) next = compileNode a (compileNode b next)
compileNode (NativeNode a) next = NativeInst a next
