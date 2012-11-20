#! /usr/bin/env runhaskell
module Main where

import System.Environment(getArgs)
import System.FilePath.Posix(takeExtension)
import Text.ParserCombinators.Parsec
import Macra.Parser
import Macra.VM
import Macra.Compiler

main = do
  args <- getArgs
  case args of
    "--nodes":fname:xs -> do
      str <- readFile fname
      putStrLn "runTimeExpr:"
      case parse runTimeExpr fname str of
           Left x -> print x
           Right x -> print x
      putStrLn "compileTimeExpr:"
      case parse compileTimeExpr fname str of
           Left x -> print x
           Right x -> print x
    "--macro":fname:xs -> do
      str <- readFile fname
      case parse compileTimeExpr fname str of
           Left x -> print x
           Right x -> print (macroDefine x)
    "--insts":fname:xs ->  do
      str <- readFile fname
      let toplevel = tail . takeExtension $ fname
      case parse compileTimeExpr fname str of
           Left x -> print x
           Right x ->
             case parse runTimeExpr fname str of
               Left x -> print x
               Right expr -> print $ compile toplevel (macroDefine x) expr
    path:xs -> do
            str <- readFile path
            let toplevel = tail . takeExtension $ path
            case parse compileTimeExpr path str of
              Left x -> print x
              Right x ->
                case parse runTimeExpr path str of
                  Left x -> print x
                  Right expr ->
                    case compile toplevel (macroDefine x) expr of
                      Right inst -> vm inst
                      Left err -> print err