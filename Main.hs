#! /usr/bin/env runhaskell
module Main where

import System.Environment(getArgs)
import qualified Macra.Parser as A

main = do
  args <- getArgs
  case args of
    "--nodes":xs -> putStrLn "Unimplemented"