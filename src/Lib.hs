module Lib
    ( WordPath(..)
    , GameData(..)
    , wordPaths
    ) where

import Control.Monad.Reader
import Data.Array
import Data.Set (fromList, Set, member)
import Data.List (tails)

data GameData = GameData { board :: Array (Int, Int) Char
                         , wordList :: Set String
                         , pref :: Set String }
data WordPath = WordPath { word :: String
                         , path :: [(Int, Int)] }
type R        = Reader GameData

direction :: (Int, Int) -> String
direction x = case x of
    (0, 0)   -> "X"
    (-1, 0)  -> "N"
    (-1, 1)  -> "NE"
    (0, 1)   -> "E"
    (1, 1)   -> "SE"
    (1, 0)   -> "S"
    (1, -1)  -> "SW"
    (0, -1)  -> "W"
    (-1, -1) -> "NW"
    _        -> "O"

dirStr :: [(Int, Int)] -> String
dirStr p = unwords $ zipWith f p (drop 1 p)
    where f (a, b) (c, d) = direction (c-a, d-b)

instance Show WordPath where
    show (WordPath s p) = unwords [show (head p), dirStr p, s]

getNeighbours :: (Int, Int) -> [(Int, Int)] -> R [(Int, Int)]
getNeighbours i@(x, y) p = do
    bs <- asks (bounds . board)
    return $ do
        x' <- [-1, 0, 1]
        y' <- [-1, 0, 1]
        let i' = (x+x', y+y')
        guard (i /= i' && inRange bs i' && i' `notElem` p)
        return i'

pathToWord :: [(Int, Int)] -> R String
pathToWord p = asks (($ p) . map . (!) . board)

run :: (Int, Int) -> [(Int, Int)] -> R [[(Int, Int)]]
run i p = do
    ns <- getNeighbours i p
    let p' = i : p
    pr <- asks pref
    w  <- pathToWord p'
    if member w pr
        then (p':) . concat <$> mapM (`run` p') ns
        else return []

getPaths' :: R [WordPath]
getPaths' = do
    GameData b w _ <- ask
    ps <- concat <$> mapM (`run` []) (indices b)
    return $ filter ((`member` w) . word) $ zipWith WordPath (map (map (b!)) ps) ps

arrFromChars :: [Char] -> Int -> Int -> Array (Int, Int) Char 
arrFromChars cs w h = array ((0,0),(h-1,w-1)) 
    [((i,j), cs !! (j+i*w)) | i <- [0..w-1], j <- [0..h-1]]

prefSet :: [String] -> Set String
prefSet = fromList . concatMap tails

wordPaths :: String -> [String] -> [WordPath]
wordPaths s ws = runReader getPaths' $ GameData
    (arrFromChars s 4 4)
    (fromList ws)
    (prefSet ws)
