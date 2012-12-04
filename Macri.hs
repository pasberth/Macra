#! /usr/bin/env runhaskell
module Main where

import qualified Data.Map as M
import Text.ParserCombinators.Parsec
import Macra.Parser
import Macra.VM
import Macra.Compiler

prompt :: String
prompt = "macri> "

macri :: MacroMap -> IO ()
macri mm = do

  putStr prompt
  input <- getLine
  case parse runTimeExpr "(macri)" input of
    -- ランタイムのコードとして valid ならランタイムのコード
    Right node ->
      case compile mm node of
        Left err -> print err >> macri mm
        -- 今は vm が終了した後、 VM の状態を得る方法がない
        Right inst -> vm inst >> macri mm
    -- そうでなければコンパイル時のコード
    Left _ ->
      case parse compileTimeExpr "(macri)" input of
        Left err -> print err >> macri mm
        Right cnode -> do
          r <- mkMacroMap cnode
          case r of
            Right mm' -> macri (mm' `M.union` mm)
            Left err -> print err >> macri mm
main = macri M.empty