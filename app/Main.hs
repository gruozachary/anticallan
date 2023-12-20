module Main (main) where

import Data.List (sortBy)
import Data.Ord (comparing)
import System.Environment (getArgs)

import Lib

main :: IO ()
main = do
    (b:f:_) <- getArgs
    ws <- lines <$> readFile f
    let ps = wordPaths b ws
    mapM_ print $ sortBy (comparing (length . word)) ps
