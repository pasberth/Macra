module Macra.Compiler (MacroMap,
                       mkMacroMap,
                       compile,
                       macroExpand,
                       emptyMacroMap) where

import qualified Data.Map as M
import qualified Control.Monad.State as S
import Control.Applicative
import qualified Text.ParserCombinators.Parsec as Parsec
import Macra.Parser hiding (Identifier)
import Macra.VM hiding (Identifier)
import qualified Macra.Finder as F
import qualified Macra.Parser as P
import qualified Macra.VM as VM


-- コンテキスト & id とマクロの関連。
-- macroDefine で作り、 macroExpand で使用する。
-- たとえば
-- #[ m a : t -> u = a ]
-- であれば CxtId は "u" で Identifier は "m" となる。
type MacroMap = M.Map (P.CxtId, P.Identifier) Macro

-- たとえば
-- #[ m a : t -> u = a ]
-- であれば ("t", "a", (PrimMNode (SymNode "a")))
-- のような形。 
type Macro = (P.CxtId, P.Identifier, MNode)

data MNode = SymMNode P.Identifier
           | CharMNode Char
           | NumMNode  Double
           | NilMNode
           | IfMNode MNode MNode MNode
           | LambdaMNode P.Identifier MNode
           | DefineMNode P.Identifier MNode
           |  FuncallMNode MNode MNode
           | PrintMNode MNode
           | ConsMNode MNode MNode
           | CarMNode MNode
           | CdrMNode MNode
           | DoMNode MNode MNode
           | NativeMNode Integer
           | EqualMNode MNode MNode
           | ReplacedMNode MNode
           | UnreplacedMNode Macro
           | ErrorMNode ExpandError
           deriving (Eq, Show)

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

data DefineError = DefineError
                 deriving (Eq, Show)

emptyMacroMap :: MacroMap
emptyMacroMap = M.fromList []

nodeToMNode :: Node -> MNode
nodeToMNode (FuncallNode node1 node2) = FuncallMNode (nodeToMNode node1) (nodeToMNode node2)

mkMacroMap :: [CNode] -> IO (Either DefineError MacroMap)
mkMacroMap [] = return $ Right emptyMacroMap
mkMacroMap xs = do { r <- include (reverse xs)
                   ; return (pure M.union <*> (define (reverse xs)) <*> r)
                   }

include :: [CNode] -> IO (Either DefineError MacroMap)
include [] = return $ Right emptyMacroMap
include (x:xs) = do { result <- include xs
                    ; case result of
                      Right mm -> include' mm x
                      Left err -> return $ Left err
                    }
               where include' mm (IncludeCNode path) = do
                       path <- F.findLib path
                       str <- readFile path
                       case Parsec.parse compileTimeExpr path str of
                         Right cnode -> mkMacroMap cnode
                         Left err -> return $ Left DefineError
                     include' mm _ = return $ Right mm
define :: [CNode] -> Either DefineError MacroMap
define [] = Right emptyMacroMap
define (x:xs) = (define xs) >>= flip define' x
              where define' :: MacroMap -> CNode -> Either DefineError MacroMap
                    define' mm (MacDefCNode id sig params node) =
                      Right $ M.insert ((last sig), id) (curry (init sig) params node) mm
                    define' mm _ = Right mm
                    curry (cxt:[]) (param:[]) node = (cxt, param, (nodeToMNode node))
                    curry (cxt:sig) (param:params) node = (cxt, param, (UnreplacedMNode (curry sig params node)))

toplevelContext = "*"

macroExpand :: MacroMap -> P.CxtId -> Node -> Either ExpandError Node
macroExpand mm cxt node = check (macroExpandC mm cxt (nodeToMNode node))
                        where check (UnreplacedMNode _) = Left ExpandError
                              check (SymMNode sym) = Right (SymNode sym)
-- MNode を展開する。
-- 戻り値の MNode に UnreplacedMNode は含まれているべきでない.
-- 含まれている場合はエラーにすべき.
macroExpandC :: MacroMap -> P.CxtId -> MNode -> MNode
macroExpandC mm cxt mnode@(SymMNode id) =
  case M.lookup (cxt, id) mm of
    Just macro -> UnreplacedMNode macro
    Nothing -> mnode
macroExpandC _ _ mnode@NilMNode = mnode
macroExpandC _ _ mnode@(CharMNode _) = mnode
macroExpandC _ _ mnode@(NumMNode _) = mnode
macroExpandC mm _ (IfMNode a b c) =
  IfMNode (macroExpandC mm toplevelContext a)
          (macroExpandC mm toplevelContext b)
          (macroExpandC mm toplevelContext c)
macroExpandC mm _ (LambdaMNode var b) =
  LambdaMNode var (macroExpandC mm toplevelContext b)
macroExpandC mm _ (DefineMNode var b) =
  DefineMNode var (macroExpandC mm toplevelContext b)
macroExpandC mm _ (PrintMNode a) =
  PrintMNode (macroExpandC mm toplevelContext a)
macroExpandC mm _ (ConsMNode a b) =
  ConsMNode (macroExpandC mm toplevelContext a)
            (macroExpandC mm toplevelContext b)
macroExpandC mm _ (CarMNode a) =
  CarMNode (macroExpandC mm toplevelContext a)
macroExpandC mm _ (CdrMNode a) =
  CdrMNode (macroExpandC mm toplevelContext a)
macroExpandC mm _ (DoMNode a b) =
  DoMNode (macroExpandC mm toplevelContext a)
         (macroExpandC mm toplevelContext b)
macroExpandC mm _ (EqualMNode a b) =
  EqualMNode (macroExpandC mm toplevelContext a)
             (macroExpandC mm toplevelContext b)
macroExpandC mm cxt (FuncallMNode n1 n2) =
  case macroExpandC mm cxt n1 of
    UnreplacedMNode (cxt', param, mnode) ->
      macroExpandC mm cxt (macroReplace mnode param (macroExpandC mm cxt' n2))
    mnode -> FuncallMNode mnode (macroExpandC mm toplevelContext n2)
macroExpandC mm cxt mnode@(UnreplacedMNode _) = mnode
macroExpandC mm cxt mnode@(ReplacedMNode _) = mnode


macroReplace :: MNode -> P.Identifier -> MNode -> MNode
macroReplace mnode@(SymMNode sym) param arg
             | param == sym = arg
             | otherwise = mnode
macroReplace NilMNode _ _ = NilMNode
macroReplace mnode@(CharMNode _) _ _ = mnode
macroReplace mnode@(NumMNode _) _ _ = mnode
macroReplace (FuncallMNode a b) param arg =
  FuncallMNode (macroReplace a param arg)
               (macroReplace b param arg)
macroReplace (IfMNode a b c) param arg =
  IfMNode (macroReplace a param arg)
         (macroReplace b param arg)
         (macroReplace c param arg)
macroReplace (LambdaMNode var b) param arg =
  LambdaMNode (macroReplaceSym var param arg)
              (macroReplace b param arg)
macroReplace (DefineMNode var b) param arg =
  DefineMNode (macroReplaceSym var param arg)
              (macroReplace b param arg)
macroReplace (PrintMNode a) param arg =
  PrintMNode (macroReplace a param arg)
macroReplace (ConsMNode a b) param arg =
  ConsMNode (macroReplace a param arg)
            (macroReplace b param arg)
macroReplace (CarMNode a) param arg =
  CarMNode (macroReplace a param arg)
macroReplace (CdrMNode a) param arg =
  CdrMNode (macroReplace a param arg)
macroReplace (DoMNode a b) param arg =
  DoMNode (macroReplace a param arg)
          (macroReplace b param arg)
macroReplace (EqualMNode a b) param arg =
  EqualMNode (macroReplace a param arg)
             (macroReplace b param arg)

macroReplaceSym :: P.Identifier -> P.Identifier -> MNode -> P.Identifier
macroReplaceSym var param (SymMNode arg)
                | param == var = arg
                | otherwise = var

macroReplaceSym var param arg
                -- たとえば
                --   #[ a => b : t = !lambda a b  ] と定義して、
                -- (1, 2) => x が !lambda (1, 2) x に展開されてしまった場合など。
                | param == var = error (concat [var, "を", show arg, "で置換しようとしました"])
                | otherwise = var

compile :: MacroMap -> Node -> Either CompileError Inst 
compile mm x =
        case  (macroExpand mm toplevelContext x) of
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
compileNode (EqualNode a b) next = compileNode a (ArgInst (compileNode b (EqualInst next)))
compileNode (NativeNode a) next = NativeInst a next
