module Main (main) where

import Data.List (sortBy)
import Data.Ord (comparing)
import System.Environment (getArgs)

import Game (wordPaths)
import WordPath (WordPath(..))
import Board (Board(..), fromLinearString, tracePath)

printWord :: WordPath -> Board -> IO ()
printWord wp@(WordPath w _) b = putStr $ unlines ["~"++w++"~", tracePath wp b]

main :: IO ()
main = do
    (b:f:_) <- getArgs
    ws <- lines <$> readFile f
    let board = fromLinearString (4, 4) b
        wps   = wordPaths board ws
    mapM_ (`printWord` board) $ sortBy (comparing (length . word)) wps
