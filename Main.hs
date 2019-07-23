{-# LANGUAGE TupleSections #-}
module Main where
  
import Distribution.Helper
import Distribution.Helper.Discover

import System.Environment
import System.FilePath
import System.Directory
import System.IO

import qualified Data.Map as M
import Data.Monoid
import Data.Foldable (toList)
import Data.List (inits, intersperse, sortOn)
import Data.Maybe (listToMaybe)
import Data.Ord
import Debug.Trace

main :: IO ()
main = do
  [root',file'] <- getArgs
  root <- canonicalizePath root'
  file <- canonicalizePath file'
  let file_dir = makeRelative root $ takeDirectory file
  mapM_ (hPutStrLn stderr) [root, file_dir]
  projs <- findProjects root
  case projs of
    (Ex proj:_) -> do
      let [dist_dir] = findDistDirs proj
      env <- mkQueryEnv proj dist_dir
      units <- runQuery (allUnits id) env
      -- mapM_ (hPutStrLn stderr . show) units
      case (getFlags file_dir $ toList units) of
        Just fs -> mapM_ putStr (showForSh $ fs++[file]) >> putStrLn ""

showForSh :: [String] -> [String]
showForSh = intersperse " " -- . map (\x -> "'"++x++"'")

getFlags :: FilePath -> [UnitInfo] -> Maybe [String]
getFlags dir uis
  = listToMaybe
  $ map (ciGhcOptions . snd)
  $ filter (hasParent dir . fst)
  $ sortOn (Down . length . fst)
  $ concatMap (\ci -> map (,ci) (ciSourceDirs ci))
  $ concat
  $ M.elems . uiComponents <$> uis

hasParent :: FilePath -> FilePath -> Bool
hasParent child parent = any (equalFilePath parent) (map joinPath $ inits $ splitPath child)
