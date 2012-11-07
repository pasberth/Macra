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
    "--insts":str:xs ->  case parse "(fname)" str of
                             Left x -> print x
                             Right x -> print $ compile (macroDefine x)
                                                        (signatureDefine x)
                                                        x
    "--eval":str:xs ->  case parse "(fname)" str of
                             Left x -> print x
                             Right x -> vm (compile (macroDefine x)
                                                    (signatureDefine x)
                                                    x)
    path:xs -> do
            str <- readFile path
            case parse path str of
                       Left x -> print x
                       Right x -> vm (compile (macroDefine x)
                                              (signatureDefine x)
                                              x)
