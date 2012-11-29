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

-- MACRALIB を探索する
-- もし fname が見つかれば f に絶対パスを与え、それを返す
-- 見つからなければ x を返す。
searchLib :: a -> (FilePath -> a) -> FilePath -> IO a
searchLib x f fname = getMacraLib >>= searchLib' x f fname
  where searchLib' x f fname [] = return x
        searchLib' x f fname (path:paths) = do
          let target = joinPath [path, fname]
          isExist <- doesFileExist target
          if isExist
            then return $ f target
            else searchLib' x f fname paths

-- MACRALIB にファイルが存在するなら真
doesLibExist :: FilePath -> IO Bool
doesLibExist fname = searchLib False (\path -> True) fname

-- MACRALIB にファイルが存在するならそのファイルの絶対パスを返す
findLib :: FilePath -> IO FilePath
findLib fname = searchLib (error ("no such file or directory: " ++ fname))
                          (\path -> path)
                          fname