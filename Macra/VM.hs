module Macra.VM (Value(..), Identifier(..), Inst(..), vm) where

import qualified Data.Map as M
import qualified Control.Monad.State as S

data Value = Double Double
           | Char Char
           | List [Value]
           | Closure Identifier Inst
           deriving (Show, Eq, Ord)

data Identifier = Sym String | Nil deriving (Show, Eq, Ord)

data Inst = FrameInst  Inst       Inst      --hasnext
          | ConstExpr  Value      Inst      --hasnext
          | ArgInst    Inst                 --hasnext
          | CloseInst  Identifier Inst Inst --hasnext
          | ApplyInst
          | ReferInst  Identifier Inst      --hasnext
          | ReturnInst
          | TestInst   Inst       Inst Inst --hasnext
          | DefineInst Identifier Inst      --hasnext
          | HaltInst
          | PrintInst  Inst                 --hasnext
          deriving (Show, Eq, Ord)

data VM = VM {
     vmAcc :: Value
   , vmInst :: Inst
   , vmEnv :: Env
   , vmRib :: Rib
   , vmStack :: Stack
     } deriving (Show)

type Env = M.Map Identifier Value
type Rib = [Value]
type Stack = [(Inst, Env, Rib)]
type VMCommand = S.StateT VM IO ()

nil :: Value
nil = List []
initialVM = VM {
          vmAcc = nil
        , vmInst = HaltInst
        , vmEnv = M.fromList []
        , vmRib = []
        , vmStack = []
          }

vm :: Inst -> IO ()
vm inst = S.evalStateT vm' initialVM { vmInst = inst }

vm' :: VMCommand
vm' = do
  vmState <- S.get
  case vmState of
    VM a HaltInst e r s -> do
      S.liftIO $ print vmState
      return ()
    VM a (ConstExpr val nxt) e r s -> do
      S.put vmState {
            vmAcc = val
          , vmInst = nxt
            }
      vm'
    VM a (PrintInst nxt) e r s -> do
      S.liftIO $ print a
      S.put vmState {
            vmInst = nxt
            }
      vm'
    VM a (ReferInst id nxt) e r s -> do
      case M.lookup id e of
        Just v -> do
          S.put vmState {
                vmAcc = v
              , vmInst = nxt
                }
          vm'
        Nothing -> do
          S.liftIO $ do
            putStr $ concat ["unbound variable: `", show id, "'"]
          return ()
    VM a (DefineInst id nxt) e r s -> do
      S.put vmState {
            vmEnv = M.insert id a e
          , vmInst = nxt
            }
      vm'
    VM a (FrameInst ret nxt) e r s -> do
      S.put vmState {
            vmStack = (ret, e, r):s
          , vmInst = nxt
            }
      vm'
    VM a (ArgInst nxt) e r s -> do
      S.put vmState {
            vmRib = a:r
          , vmInst = nxt
            }
      vm'
    VM a ReturnInst _ _ ((ret, e, r):s) -> do
      S.put vmState {
            vmInst = ret
          , vmEnv = e
          , vmRib = r
          , vmStack = s
            }
      vm'
    VM a ReturnInst e r [] -> do
      S.liftIO $ do
        putStr $ concat ["stack is empty"]
      return ()
    VM a (CloseInst var body nxt) e r s -> do
      S.put vmState {
            vmAcc = Closure var body
          , vmInst = nxt
            }
      vm'