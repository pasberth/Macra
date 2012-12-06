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
searchLib :: a -> (FilePath -> a) -> FilePath -> IO a
searchLib x f fname = getMacraLib >>= searchLib' x f fname
  where -- 見つからなければ x を返す。
        searchLib' x f fname [] = return x
        -- もし fname が見つかれば f に fname の 絶対パスを与え、それを返す
        searchLib' x f fname (path:paths) = do
          let target = joinPath [path, fname]
          isExist <- doesFileExist target
          if isExist
            then return $ f target
            else putMacraExt x f fname (path:paths)
        -- fname が見つからず、もし fname.macra が存在するなら f に fname.macra の絶対パスを与え、それを返す
        putMacraExt x f fname (path:paths)
          -- ただし、 fname がもともと .macra の拡張子であればこの手順はスキップする
          | takeExtension fname == ".macra" = searchLib' x f fname paths
          | otherwise = do
            let target = addExtension (joinPath [path, fname]) "macra"
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