#! /usr/bin/env runhaskell
module Main where

import System.Environment(getArgs)
import Macra.Parser
import Macra.VM
import Macra.Compiler

main = do
  args <- getArgs
  case args of
    "--nodes":fname:xs -> do
      str <- readFile fname
      case parse fname str of
           Left x -> print x
           Right x -> print x
    "--macro":fname:xs -> do
      str <- readFile fname
      case parse fname str of
           Left x -> print x
           Right x -> print (macroDefine x)
    "--signa":fname:xs -> do
      str <- readFile fname
      case parse fname str of
           Left x -> print x
           Right x -> print (signatureDefine x)
    "--insts":fname:xs ->  do
      str <- readFile fname
      case parse fname str of
           Left x -> print x
           Right x -> print $ compile (macroDefine x)
                                      (signatureDefine x)
                                      x
    "--eval":fname:xs -> do
      str <- readFile fname
      case parse fname str of
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
