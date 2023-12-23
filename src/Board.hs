module Board
    ( Board(..)
    , fromLinearString
    , tracePath
    ) where

import Data.List (intercalate)
import Data.Array (Array, array, (//), bounds, (!))
import WordPath (WordPath, dirPosPairs, dirChar)
import Control.Arrow (second)

newtype Board = Board (Array (Int, Int) Char)

fromLinearString :: (Int, Int) -> String -> Board
fromLinearString (w, h) cs = Board $ array ((0,0),(h-1,w-1)) 
    [((i,j), cs !! (j+i*w)) | i <- [0..w-1], j <- [0..h-1]]

prettyArr :: Array (Int, Int) Char -> String
prettyArr xs = intercalate "\n" [[xs ! (i,j) | j <- [0..w]] | i <- [0..h]]
    where (_, (w, h)) = bounds xs

instance Show Board where
    show (Board xs) = prettyArr xs

tracePath :: WordPath -> Board -> String
tracePath wp (Board xs) = prettyArr $ xs // map (second dirChar) (dirPosPairs wp)
