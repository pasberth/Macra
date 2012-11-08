module Macra.VM (Value(..), Identifier(..), Inst(..), vm) where

import qualified Data.Unique as U
import qualified Data.Map as M
import qualified Control.Monad.State as S

data Value = Double Double
           | Char Char
           | List [Value]
           | Closure Identifier Inst EnvRef
           deriving (Eq, Ord)

instance Show Value where
  show (Char c) = [c]
  show (Double i) = show i
  show (List xs) = concat ["(", concat (map (\x -> (show x) ++ ";") xs), ")"]
  show (Closure var body e) = concat [show var, show body]

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
   , vmEnvRef :: EnvRef
   , vmRib :: Rib
   , vmStack :: Stack
   , vmEnvMem :: EnvMem
     }

instance Show VM where
  show (VM a x envRef r s envMem) = concat ["A: ", show a, "\n",
                                            "X: ", show x, "\n",
                                            "E: ", (case M.lookup envRef envMem of
                                                     Just e -> show e
                                                     Nothing -> "Environment Reference Error"), "\n",
                                            "R: ", show r, "\n",
                                            "S: ", "TODO", "\n"]


type Env = M.Map Identifier Value
type Rib = [Value]
type Stack = [(Inst, EnvRef, Rib)]
type VMCommand = S.StateT VM IO ()

type EnvRef = U.Unique
type EnvMem = M.Map EnvRef Env

nil :: Value
nil = List []

vm :: Inst -> IO ()
vm inst = do
        envRef <- U.newUnique
        S.evalStateT vm' VM {
                         vmAcc = nil
                       , vmInst = inst
                       , vmEnvRef = envRef
                       , vmRib = []
                       , vmStack = []
                       , vmEnvMem = M.fromList [(envRef, M.fromList [])]
                         }

vm' :: VMCommand
vm' = do
  vmState <- S.get
  case vmState of
    VM a HaltInst e r s _ -> do
      return ()
    VM a (ConstExpr val nxt) e r s _ -> do
      S.put vmState {
            vmAcc = val
          , vmInst = nxt
            }
      vm'
    VM a (PrintInst nxt) e r s _ -> do
      S.liftIO $ print a
      S.put vmState {
            vmInst = nxt
            }
      vm'
    VM a (ReferInst id nxt) envRef r s envMem -> do
      case M.lookup envRef envMem of
        Just e -> do
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
    VM a (DefineInst id nxt) envRef r s mem -> do
      case M.lookup envRef mem of
        Just e -> do
          S.put vmState {
                vmEnvMem = M.insert envRef (M.insert id a e) mem
              , vmInst = nxt
                }
          vm'
          return ()
    VM a (FrameInst ret nxt) envRef r s _ -> do
      S.put vmState {
            vmStack = (ret, envRef, r):s
          , vmInst = nxt
            }
      vm'
    VM a (ArgInst nxt) e r s _ -> do
      S.put vmState {
            vmRib = a:r
          , vmInst = nxt
            }
      vm'
    VM a ApplyInst _ (val:r) s mem -> do
      case a of
        (Closure (Sym var) body envRef) -> do
          closedEnvRef <- S.lift U.newUnique
          case M.lookup envRef mem of
            Just e -> do
              S.put vmState {
                    vmEnvRef = closedEnvRef
                  , vmEnvMem = (M.insert closedEnvRef
                                         (M.insert (Sym var) val e)
                                         mem)
                  , vmInst = body
                    }
              vm'
            Nothing -> do
              return ()
        (Closure Nil body envRef) -> do
          S.put vmState {
                vmEnvRef = envRef
              , vmInst = body
                }
          vm'
        _ -> do
          S.liftIO $ do
            putStr $ concat ["invalid application: ", show a]
          return ()
    VM a ReturnInst _ _ ((ret, envRef, r):s) _ -> do
        S.put vmState {
              vmInst = ret
            , vmEnvRef = envRef
            , vmRib = r
            , vmStack = s
              }
        vm'
    VM a ReturnInst e r [] _ -> do
      S.liftIO $ do
        putStr $ concat ["stack is empty"]
      return ()
    VM a (CloseInst var body nxt) envRef r s mem -> do
        S.put vmState {
              vmAcc = Closure var body envRef
            , vmInst = nxt
              }
        vm'
    _ -> do
     S.liftIO $ do
       print "** VM BUG **: "
       print vmState