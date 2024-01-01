module Game
    ( wordPaths
    ) where

import Control.Monad.Reader
import Data.Array
import Data.Set (fromList, Set, member)
import Data.List (tails, nubBy)
import Data.Function (on)
import Control.Applicative (liftA2)

import Board(Board(..))
import WordPath

data GameData = GameData { board :: Array (Int, Int) Char
                         , wordList :: Set String
                         , pref :: Set String }
type R        = Reader GameData

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
    return $ nubBy ((==) `on` word)
           $ filter (liftA2 (&&) ((2<) . length) (`member` w) . word)
           $ zipWith WordPath (map (map (b!)) ps) ps

prefSet :: [String] -> Set String
prefSet = fromList . concatMap tails

wordPaths :: Board -> [String] -> [WordPath]
wordPaths (Board xs) ws = runReader getPaths' $ GameData
    xs
    (fromList ws)
    (prefSet ws)
