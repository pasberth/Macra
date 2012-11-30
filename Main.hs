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
           Right x -> do
             r <- mkMacroMap x
             case r of
               Right mm -> print mm
               Left err -> print err
    "--insts":fname:xs ->  do
      str <- readFile fname
      case parse compileTimeExpr fname str of
           Left x -> print x
           Right x ->
             case parse runTimeExpr fname str of
               Left x -> print x
               Right expr -> do
                 r <- mkMacroMap x
                 case r of
                   Right mm -> print $ compile mm expr >>= (\inst -> return $ optimize inst)
                   Left err -> print err
    path:xs -> execFile path


execFile path = do
  str <- readFile path
  let parseCompileTimeExpr = case parse compileTimeExpr path str of
                                    Left x -> print x
                                    Right cnode -> parseRunTimeExpr cnode
      parseRunTimeExpr cnode = case parse runTimeExpr path str of
                                      Left x -> print x
                                      Right node -> compileNode cnode node
      compileNode cnode node = do
                                 r <- mkMacroMap cnode
                                 case r of
                                   Left err -> print err
                                   Right mm ->
                                     case compile mm node of
                                       Right inst -> execInst inst
                                       Left err -> print err
      execInst inst = vm $ optimize inst
  parseCompileTimeExpr