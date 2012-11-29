module Macra.Finder ( getMacraLib,
                      doesLibExist,
                      findLib ) where

-- import Control.Exception
import System.Environment
import System.Directory
import System.FilePath.Posix
import Data.List.Split -- $ cabal install split

type MacraLib = [FilePath]

-- MACRALIB をコロン区切りで返す
getMacraLib :: IO MacraLib
getMacraLib = do {
  str <- getEnv "MACRALIB"
; return $ splitOn ":" str
} `catch` (\_ -> return [])

-- MACRALIB にファイルが存在するなら真
doesLibExist :: FilePath -> IO Bool
doesLibExist fname = getMacraLib >>= doesLibExist' fname
  where  doesLibExist' fname [] = return False
         doesLibExist' fname (path:paths) = do
           isExist <- doesFileExist (joinPath [path, fname])
           if isExist
             then return True
             else doesLibExist' fname paths

-- MACRALIB にファイルが存在するならそのファイルの絶対パスを返す
findLib :: FilePath -> IO FilePath
findLib fname = getMacraLib >>= findLib' fname
  where  findLib' fname [] = error ("no such file or directory: " ++ fname)
         findLib' fname (path:paths) = do
           isExist <- doesFileExist (joinPath [path, fname])
           if isExist
             then return (joinPath [path, fname])
             else findLib' fname paths