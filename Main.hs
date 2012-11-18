#! /usr/bin/env runhaskell
module Main where

import System.Environment(getArgs)
import Text.ParserCombinators.Parsec
import Macra.Parser
import Macra.VM
import Macra.Compiler

main = do
  args <- getArgs
  case args of
    "--nodes":fname:xs -> do
      str <- readFile fname
      case parse runTimeExpr fname str of
           Left x -> print x
           Right x -> print x
    "--macro":fname:xs -> do
      str <- readFile fname
      case parse compileTimeExpr fname str of
           Left x -> print x
           Right x -> print (macroDefine x)
    "--insts":fname:xs ->  do
      str <- readFile fname
      case parse compileTimeExpr fname str of
           Left x -> print x
           Right x ->
             case parse runTimeExpr fname str of
               Left x -> print x
               Right expr -> print $ compile (macroDefine x) expr
    path:xs -> do
            str <- readFile path
            case parse compileTimeExpr path str of
              Left x -> print x
              Right x ->
                case parse runTimeExpr path str of
                  Left x -> print x
                  Right expr ->
                    case compile (macroDefine x) expr of
                      Right inst -> vm inst
                      Left err -> print err