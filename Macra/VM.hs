module Macra.VM (Value(..), Identifier(..), Inst(..), vm) where

import qualified Data.Unique as U
import qualified Data.Map as M
import qualified Data.List as L
import qualified Control.Monad.State as S

data Value = Double Double
           | Char Char
           | List [Value]
           | Refered Value Identifier         --Pair of value and the refered key, used in thaw
           | Closure Identifier Inst   EnvRef
           | Thunk   Inst              EnvRef
           deriving (Eq, Ord)

instance Show Value where
  show (Char c) = [c]
  show (Double i) = show i
  show (List xs) = concat ["(", concat (L.intersperse " " (map show xs)), ")"]
  show (Closure var body e) = concat [show "Close: ", show var, show body]
  show (Thunk body e) = show body
  show (Refered val idf) = concat [show idf, show ":", show val]

type Identifier = String
data Inst = FrameInst  Inst       Inst           --hasnext
          | ConstExpr  Value      Inst           --hasnext
          | ConsInst   Inst                      --hasnext
          | CarInst    Inst                      --hasnext
          | CdrInst    Inst                      --hasnext
          | ArgInst    Inst                      --hasnext
          | CloseInst  Identifier Inst Inst      --hasnext
          | FreezeInst Inst       Inst           --hasnext
          | ApplyInst
          | ThawInst   Inst                      --hasnext
          | ReferInst  Identifier Inst           --hasnext
          | ReturnInst
          | TestInst   Inst       Inst
          | DefineInst Identifier Inst           --hasnext
          | HaltInst
          | PrintInst  Inst                      --hasnext
          | NativeInst Integer Inst                  --hasnext
          -- | NativeCallInst TwoArgFn Inst         --hasnext
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

nativeFunction nativeId =
  -- nativeIdは1から始まる4桁の数字とする。(なんとなく)
  -- ここは将来Mapでマッチングさせる
  case nativeId of
      1001 -> \x -> \y -> Double (x + y) -- Mathematical add
      1002 -> \x -> \y -> Double (x - y) -- Mathematical sub
      1003 -> \x -> \y -> Double (x * y) -- Mathematical mul
      1004 -> \x -> \y -> Double (x `div` y) -- Mathematical div
      1005 -> \x -> \y -> Double (x `mod` y) -- Mathematical mod


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
                                                 , (initialEnv, dummyParentEnvRef)
                                                 ) ]
                         }
        where initialEnv = M.fromList [ ("nil", nil) ]



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
vm'' vmState@(VM a (ConsInst nxt) e (atom:r) s _) = do
     case a of
       List list -> do
            S.put vmState {
                  vmAcc = List (atom:list)
                , vmRib = r
                , vmInst = nxt
                  }
            vm'
       val -> do
         S.liftIO $ putStr $ concat [ "invalid list: ", show val ]
         return ()
vm'' vmState@(VM a (CarInst nxt) e r s _) = do
     case a of
       List (x:xs) -> do
            S.put vmState {
                  vmAcc = x
                , vmInst = nxt
                  }
            vm'
       List [] -> do
         S.liftIO $ putStr $ concat [ "!car nil" ]
         return ()
       val -> do
         S.liftIO $ putStr $ concat [ "invalid list: ", show val ]
         return ()
vm'' vmState@(VM a (CdrInst nxt) e r s _) = do
     case a of
       List (x:xs) -> do
            S.put vmState {
                  vmAcc = List xs
                , vmInst = nxt
                  }
            vm'
       List [] -> do
         S.liftIO $ putStr $ concat [ "!cdr nil" ]
         return ()
       val -> do
         S.liftIO $ putStr $ concat [ "invalid list: ", show val ]
         return ()
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
                vmAcc = Refered v id --thaw needs id
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

-- ApplyInst applies a Closure to an argument
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

-- ThawInst thaws thunks
-- Call-by-name strategy thaws the thunk everytime the name is seen
vm'' vmState@(VM (Refered a id) (ThawInst nxt) fnEnvRef _ _ mem) = do
  case a of
    (Thunk body envRef) -> do
      S.put vmState {
          vmAcc = a
        , vmInst = body  -- evaluate the thunk
        , vmEnvRef = envRef
      }
      vm'
      newVMState@(VM acc _ _ _ _ mem') <- S.get
      closedEnvRef <- S.lift U.newUnique
      S.put newVMState {
            vmInst = nxt   -- go to next instruction with the evaluated thunk on the accum
          , vmEnvRef = closedEnvRef
          , vmEnvMem = (M.insert closedEnvRef
                                 ((M.fromList [ (id, acc) ]), fnEnvRef)
                                 mem')
      }
      vm'
    _ -> do
      -- pass if the symbol is bound to non-thunk
      S.put vmState {
            vmAcc = a
          , vmInst = nxt
      }
      vm'

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
vm'' vmState@(VM a (TestInst thenExp elseExp) e r s mem)
     | a == nil = do
       S.put vmState {
             vmInst = elseExp
             }
       vm'
     | otherwise = do
       S.put vmState {
             vmInst = thenExp
             }
       vm'
vm'' vmState@(VM a (CloseInst var body nxt) envRef r s mem) = do
        S.put vmState {
              vmAcc = Closure var body envRef
            , vmInst = nxt
              }
        vm'
        
-- Thunk is Closure without param
vm'' vmState@(VM a (FreezeInst body nxt) envRef r s mem) = do
        S.put vmState {
              vmAcc = Thunk body envRef
            , vmInst = nxt
              }
        vm'

-- native funcations --
vm'' vmState@(VM _ (NativeInst nativeId nxt) _ ((Thunk (ConstExpr  (Double x) _) _):(Thunk (ConstExpr (Double y) _) _):r) _ _) = do
        S.put vmState {
              vmAcc = (nativeFunction nativeId) x y
            ,  vmInst = nxt
        }
        vm'

vm'' vmState = do
     S.liftIO $ do
       print "** VM BUG **: "
       print vmState