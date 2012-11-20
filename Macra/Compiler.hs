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
                 | ExpandArgumentError Macro
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
                            , (MacroNode (SymNode id))) mm
macroDefineMacCxtNode mm (Shebang _ _) = mm

macroExpand :: P.CxtId -> MacroMap -> P.CxtId -> Node -> Either ExpandError Node
macroExpand toplevelContext mm cxt node =
  case macroExpandCurry toplevelContext mm cxt node of
    Right ([], [], node) -> Right node
    Right macro -> Left $ ExpandArgumentError macro
    Left err -> Left err

macroExpandCurry :: P.CxtId -> MacroMap -> P.CxtId -> Node -> Either ExpandError Macro

macroExpandCurry _ _ _ node@(MacroNode _) = Right ([], [], node)

-- シンボルだった場合、 CxtId に関連付けられた
-- マクロを、 (MacSig, MacParams, Node) の形式で返す。
-- macroExpandCurry を呼んだ側で、 MacSig と MacParams 従って Node を 置換する。
macroExpandCurry _ mm cxtId node@(SymNode macroId) =
  case M.lookup (cxtId, macroId) mm of
    Just macro -> Right macro
    Nothing -> Right ([], [], node)

macroExpandCurry toplevelContext mm cxtId node@(FuncallNode a b) =
  -- macroExpandCurry でカリー化されたマクロを得る
  case macroExpandCurry toplevelContext mm cxtId a of
    Left err -> Left err
    -- TODO: Right macro@([], param:params, node) -> Left $ ExpandArgumentError macro
    -- for example, when
    -- #[ f x y : a -> b = .. ]

    -- もし aが すべての引数が適用済みのマクロであれば、
    -- b はそのまま a の関数の引数とする。
    Right ([], [], fn) ->
      pure (\b -> ([], [], FuncallNode fn b)) <*> macroExpand toplevelContext mm toplevelContext b
    -- もし aがすべての引数が適用済みであるが、
    -- コンテキストが余っているマクロであれば、
    -- b は関数の引数にしつつコンテキストは変える。
    Right (cxt:sig, [], fn) ->
      pure (\b -> (sig, [], FuncallNode fn b)) <*> macroExpand toplevelContext mm cxt b

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
      case pure (macroReplace param macroNode) <*> (macroExpand toplevelContext mm cxt b) of
        Right r -> case r of
                     Right node -> pure (\x -> x) <*> macroExpandCurry toplevelContext mm cxtId node
                     Left err -> Left err
        Left err -> Left err

    -- もし a が引数が適用されていないマクロであれば、
    -- a に含まれるparam をすべて b に置換する
    Right (cxt:sig, param:params, (MacroNode macroNode)) ->
      case (macroExpand toplevelContext mm cxt b) of
        Right b ->
          case (macroReplace param
                             macroNode
                             b) of
            Right node -> Right (sig, params, (MacroNode node))
            Left err -> Left err
        Left err -> Left err
macroExpandCurry toplevelContext mm _ node = pure (\node -> ([], [], node)) <*> macroExpandRecur toplevelContext mm node

-- MacroExpandRecur はたとえば
--   !cons a b
-- の a b もマクロ展開する。この a b は常に toplevel 。
macroExpandRecur :: P.CxtId -> MacroMap -> Node -> Either ExpandError Node
macroExpandRecur _ _ node@NilNode      = Right node
macroExpandRecur _ _ node@(CharNode _) = Right node
macroExpandRecur _ _ node@(NumNode _)  = Right node
macroExpandRecur _ _ node@(NativeNode _) = Right node
macroExpandRecur toplevelContext mm node@(PrintNode expr) =
  pure PrintNode <*> macroExpand toplevelContext mm toplevelContext expr
macroExpandRecur toplevelContext mm (ConsNode a b) =
  pure ConsNode <*> (macroExpand toplevelContext mm toplevelContext a)
                <*> (macroExpand toplevelContext mm toplevelContext b)
macroExpandRecur toplevelContext mm (CarNode a) =
  pure CarNode <*> (macroExpand toplevelContext mm toplevelContext a)
macroExpandRecur toplevelContext mm (CdrNode a) =
  pure CdrNode <*> (macroExpand toplevelContext mm toplevelContext a)
macroExpandRecur toplevelContext mm (IfNode condExp thenExp elseExp) =
  pure IfNode <*> macroExpand toplevelContext mm toplevelContext condExp
              <*> macroExpand toplevelContext mm toplevelContext thenExp
              <*> macroExpand toplevelContext mm toplevelContext elseExp
macroExpandRecur toplevelContext mm (DoNode a b) =
  pure DoNode <*> (macroExpand toplevelContext mm toplevelContext a)
              <*> (macroExpand toplevelContext mm toplevelContext b)
macroExpandRecur toplevelContext mm (LambdaNode param body) =
  pure (LambdaNode param) <*> macroExpand toplevelContext  mm toplevelContext body

macroExpandRecur toplevelContext mm (DefineNode id expr) =
  pure (DefineNode id) <*> macroExpand toplevelContext mm toplevelContext expr

macroReplace :: P.Identifier -> Node -> Node -> Either ExpandError Node
macroReplace param NilNode arg = Right NilNode
macroReplace param node@(MacroNode _) arg = Right node
macroReplace param node@(SymNode sym) arg
             | param == sym = Right arg
             | otherwise = Right node
macroReplace param node@(CharNode _) arg = Right node
macroReplace param node@(NumNode _) arg = Right node
macroReplace param node@(FuncallNode a b) arg =
             pure FuncallNode
                  <*> (macroReplace param a arg)
                  <*> (macroReplace param b arg)
macroReplace param node@(IfNode a b c) arg =
             pure IfNode
                  <*> (macroReplace param a arg)
                  <*> (macroReplace param b arg)
                  <*> (macroReplace param c arg)
macroReplace param node@(LambdaNode var body) arg =
             pure LambdaNode
                  <*> (macroReplaceSym param var arg)
                  <*> (macroReplace param body arg)
macroReplace param node@(DefineNode id expr) arg =
             pure DefineNode
                  <*> (macroReplaceSym param id arg)
                  <*> (macroReplace param expr arg)
macroReplace param node@(PrintNode expr) arg =
             pure PrintNode <*> (macroReplace param expr arg)
macroReplace param node@(ConsNode a b) arg =
             pure ConsNode
                  <*> (macroReplace param a arg)
                  <*> (macroReplace param b arg)
macroReplace param node@(CarNode a) arg =
             pure CarNode <*> (macroReplace param a arg)
macroReplace param (CdrNode a) arg =
             pure CdrNode <*> (macroReplace param a arg)
macroReplace param (DoNode a b) arg =
             pure DoNode <*> (macroReplace param a arg)
                         <*> (macroReplace param b arg)

macroReplaceSym :: P.Identifier -> P.Identifier -> Node -> Either ExpandError P.Identifier
macroReplaceSym param var (P.SymNode arg)
                | param == var = Right arg
                | otherwise = Right var

macroReplaceSym param var arg
                -- たとえば
                --   #[ a => b : t = !lambda a b  ] と定義して、
                -- (1, 2) => x が !lambda (1, 2) x に展開されてしまった場合など。
                -- TODO: 単に ExpandError ではなく、なにかメッセージを付ける
                | param == var = Left ExpandError
                -- たとえば
                --   #[ a => b : t = !lambda x b a ]
                -- の x は置換しなくてもいいが macroReplaceSym は呼ばれる。
                -- これは単に x を返す。
                | otherwise = Right var

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
compileNode (MacroNode node) next = compileNode node next
compileNode (ConsNode a b) next = compileNode a (ArgInst (compileNode b (ConsInst next)))
compileNode (CarNode node) next = compileNode node (CarInst next)
compileNode (CdrNode node) next = compileNode node (CdrInst next)
compileNode (DoNode a b) next = compileNode a (compileNode b next)
compileNode (NativeNode a) next = NativeInst a next
