module Macra.Finder (doesLibExist) where

-- import Control.Exception
import System.Environment
import System.Directory
import System.FilePath.Posix
import Data.List.Split -- $ cabal install split

type MacraLib = [FilePath]

getMacraLib :: IO MacraLib
getMacraLib = do {
  str <- getEnv "MACRALIB"
; return $ splitOn ":" str
} `catch` (\_ -> return [])

doesLibExist :: FilePath -> MacraLib -> IO Bool
doesLibExist fname [] = return False
doesLibExist fname (path:paths) = do
  isExist <- doesFileExist (joinPath [path, fname])
  if isExist
    then return True
    else doesLibExist fname paths