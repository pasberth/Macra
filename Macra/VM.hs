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

type Identifier = String
data Inst = FrameInst  Inst       Inst           --hasnext
          | ConstExpr  Value      Inst           --hasnext
          | ArgInst    Inst                      --hasnext
          | CloseInst  Identifier Inst Inst      --hasnext
          | ApplyInst
          | ReferInst  Identifier Inst           --hasnext
          | ReturnInst
          | TestInst   Inst       Inst Inst Inst --hasnext
          | DefineInst Identifier Inst           --hasnext
          | HaltInst
          | PrintInst  Inst                      --hasnext
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
                                                     Just (e, p) -> show e
                                                     Nothing -> "Environment Reference Error"), "\n",
                                            "R: ", show r, "\n",
                                            "S: ", "TODO", "\n"]

type Env = ((M.Map Identifier Value), EnvRef)
type Rib = [Value]
type Stack = [(Inst, EnvRef, Rib)]
type VMCommand = S.StateT VM IO ()

type EnvRef = U.Unique
type EnvMem = M.Map EnvRef Env

lookupVal :: Identifier -> EnvRef -> EnvMem -> Maybe Value
lookupVal id envRef mem =
  case M.lookup envRef mem of
    Nothing -> Nothing
    Just (e, parentEnvRef) ->
      case M.lookup id e of
         Just val -> Just val
         Nothing -> lookupVal id parentEnvRef mem

nil :: Value
nil = List []

vm :: Inst -> IO ()
vm inst = do
        dummyParentEnvRef <- U.newUnique
        envRef <- U.newUnique
        S.evalStateT vm' VM {
                         vmAcc = nil
                       , vmInst = inst
                       , vmEnvRef = envRef
                       , vmRib = []
                       , vmStack = []
                       , vmEnvMem = M.fromList [ ( envRef
                                                 , ((M.fromList []), dummyParentEnvRef)
                                                 ) ]
                         }

vm' :: VMCommand
vm' = S.get >>= vm''

vm'' ::  VM -> VMCommand
vm'' (VM a HaltInst e r s _) = return ()
vm'' vmState@(VM a (ConstExpr val nxt) e r s _) = do
      S.put vmState {
            vmAcc = val
          , vmInst = nxt
            }
      vm'
vm'' vmState@(VM a (PrintInst nxt) e r s _) = do
      S.liftIO $ print a
      S.put vmState {
            vmInst = nxt
            }
      vm'
vm'' vmState@(VM a (ReferInst id nxt) envRef r s envMem) = do
      case lookupVal id envRef envMem of
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
vm'' vmState@(VM a (DefineInst id nxt) envRef r s mem) = do
      case M.lookup envRef mem of
        -- TODO: Nothing ->
        Just (e, parentEnvRef) -> do
          S.put vmState {
                vmEnvMem = M.insert envRef
                                    ((M.insert id a e), parentEnvRef)
                                    mem
              , vmInst = nxt
                }
          vm'
          return ()
vm'' vmState@(VM a (FrameInst ret nxt) envRef r s _) = do
      S.put vmState {
            vmStack = (ret, envRef, r):s
          , vmInst = nxt
            }
      vm'
vm'' vmState@(VM a (ArgInst nxt) e r s _) = do
      S.put vmState {
            vmRib = a:r
          , vmInst = nxt
            }
      vm'
vm'' vmState@(VM a ApplyInst _ (val:r) s mem) = do
      case a of
        (Closure var body envRef) -> do
          closedEnvRef <- S.lift U.newUnique
          S.put vmState {
                vmEnvRef = closedEnvRef
              , vmEnvMem = (M.insert closedEnvRef
                                     ((M.fromList [ (var, val) ]), envRef)
                                     mem)
              , vmInst = body
                }
          vm'
        _ -> do
          S.liftIO $ do
            putStr $ concat ["invalid application: ", show a]
          return ()
vm'' vmState@(VM a ReturnInst _ _ ((ret, envRef, r):s) _) = do
        S.put vmState {
              vmInst = ret
            , vmEnvRef = envRef
            , vmRib = r
            , vmStack = s
              }
        vm'
vm'' vmState@(VM a ReturnInst e r [] _) = do
      S.liftIO $ do
        putStr $ concat ["stack is empty"]
      return ()
vm'' vmState@(VM a (CloseInst var body nxt) envRef r s mem) = do
        S.put vmState {
              vmAcc = Closure var body envRef
            , vmInst = nxt
              }
        vm'
vm'' vmState = do
     S.liftIO $ do
       print "** VM BUG **: "
       print vmState