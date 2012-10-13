#! /usr/bin/env runhaskell
module Main where

import System.Environment(getArgs)
import Macra.Parser
import Macra.VM
import Macra.Compiler

main = do
  args <- getArgs
  case args of
    "--nodes":str:xs -> case parse "(fname)" str of
                             Left x -> print x
                             Right x -> print x
    "--expand":str:xs -> case parse "(fname)" str of
                             Left x -> print x
                             Right x -> print "" -- $ expand' (define' x) x
    "--insts":str:xs ->  case parse "(fname)" str of
                             Left x -> print x
                             Right x -> print $ compile' (define' x) x
    "--eval":str:xs ->  case parse "(fname)" str of
                             Left x -> print x
                             Right x -> eval' x

  where eval' :: ToplevelNodes -> IO ()
        eval' x = vm (compile' (define' x) x)
        compile' :: MacroMap -> ToplevelNodes -> Inst 
        compile' mm ((MacCxtTLNode x):xs) = compile' mm xs
        compile' mm ((EvalCxtTLNode x):xs) = compile (expand' mm x) (compile' mm xs)
        compile' mm [] = HaltInst
        define' :: ToplevelNodes -> MacroMap
        define' ((EvalCxtTLNode x):xs) = define' xs
        define' ((MacCxtTLNode x):xs) = macroDefine (define' xs) toplevelContext x
        define' [] = emptyMacroMap
        expand' :: MacroMap -> Node -> Node
        expand' mm x = (macroExpand mm toplevelContext x)