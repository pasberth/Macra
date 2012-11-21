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
macroExpand toplevelContext mm cxt node =
  case lookupMacro mm cxt node of
    Nothing -> Right node
    Just (macro@(sig, params, macroNode), args)
      | length params > length args -> Left $ ExpandArgumentError macro args
      -- もし sig が足りなければ、すべて toplevel として扱う
      -- もし params が足りなければ、あふれた args はすべて funcall の引数として扱う
      | otherwise -> (expandArgs sigAndArgs) >>=
                     (\args -> let fargs = drop (length params) args
                               in macroReplace macroNode (zip params args) >>= (\x -> Right $ foldl FuncallNode x fargs) )
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

macroReplace :: Node -> [(P.Identifier, Node)] -> Either ExpandError Node
macroReplace node [] = Right node
macroReplace node@(SymNode sym) ((param, arg):xs)
             | param == sym = Right arg
             | otherwise = macroReplace node xs
macroReplace NilNode _ = Right NilNode
macroReplace node@(CharNode _) _ = Right node
macroReplace node@(NumNode _) _ = Right node
macroReplace (FuncallNode a b) xs =
             pure FuncallNode
                  <*> (macroReplace a xs)
                  <*> (macroReplace b xs)
macroReplace (IfNode a b c) xs =
             pure IfNode
                  <*> (macroReplace a xs)
                  <*> (macroReplace b xs)
                  <*> (macroReplace c xs)
macroReplace (LambdaNode var b) xs =
             pure LambdaNode
                  <*> (macroReplaceSym var xs)
                  <*> (macroReplace b xs)
macroReplace (DefineNode var b) xs =
             pure DefineNode
                  <*> (macroReplaceSym var xs)
                  <*> (macroReplace b xs)
macroReplace (PrintNode a) xs =
             pure PrintNode <*> (macroReplace a xs)
macroReplace (ConsNode a b) xs =
             pure ConsNode
                  <*> (macroReplace a xs)
                  <*> (macroReplace b xs)
macroReplace (CarNode a) xs =
             pure CarNode <*> (macroReplace a xs)
macroReplace (CdrNode a) xs =
             pure CdrNode <*> (macroReplace a xs)
macroReplace (DoNode a b) xs =
             pure DoNode <*> (macroReplace a xs)
                         <*> (macroReplace b xs)

macroReplaceSym :: P.Identifier -> [(P.Identifier, Node)] -> Either ExpandError P.Identifier
macroReplaceSym var [] = Right var
macroReplaceSym var ((param, (P.SymNode arg)):xs)
                | param == var = Right arg
                | otherwise = macroReplaceSym var xs

macroReplaceSym var ((param, arg):xs)
                -- たとえば
                --   #[ a => b : t = !lambda a b  ] と定義して、
                -- (1, 2) => x が !lambda (1, 2) x に展開されてしまった場合など。
                -- TODO: 単に ExpandError ではなく、なにかメッセージを付ける
                | param == var = Left ExpandError
                | otherwise = macroReplaceSym var xs

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
