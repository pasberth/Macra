module Macra.VM (Value(..), Identifier(..), Inst(..), vm) where

import qualified Data.Map as M
import qualified Control.Monad.State as S

data Value = Double Double
           | Char Char
           | List [Value]
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
          deriving (Show, Eq)

data VM = VM {
     vmAcc :: Value
   , vmInst :: Inst
   , vmEnv :: Env
   , vmRib :: Rib
   , vmStack :: Stack
     }

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
    VM a HaltInst e r s -> return ()
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
      case M.lookup id (vmEnv vmState) of
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