module Board
    ( Board(..)
    , fromLinearString
    , tracePath
    ) where

import Data.List (intercalate)
import Data.Array (Array, array, (//), bounds, (!), assocs)
import WordPath (WordPath(..), dirPosPairs, dirChar, offset, opposite)

newtype Board = Board (Array (Int, Int) Char)

tracePath :: WordPath -> Board -> String
tracePath wp (Board xs) =
    let (_, (y, x)) = bounds xs
        g (a, b) (c, d)  = (a+c, b+d)
        zs = map (\((a, b), c) -> ((2*a+1, 2*b+1), c)) (assocs xs)
        bs = map (\((a, b), c) -> (g (2*a+1, 2*b+1) (offset $ opposite c), dirChar c)) (dirPosPairs wp)
        cs = array ((0, 0), (2*y+2, 2*x+2)) [((i, j), ' ') | i <- [0..2*y+2], j <- [0..2*x+2]]
        ds = cs // (bs ++ zs)
    in prettyArr ds
    

fromLinearString :: (Int, Int) -> String -> Board
fromLinearString (w, h) cs = Board $ array ((0,0),(h-1,w-1)) 
    [((i,j), cs !! (j+i*w)) | i <- [0..w-1], j <- [0..h-1]]

prettyArr :: Array (Int, Int) Char -> String
prettyArr xs = intercalate "\n" [[xs ! (i,j) | j <- [x..w]] | i <- [y..h]]
    where ((x, y), (w, h)) = bounds xs

instance Show Board where
    show (Board xs) = prettyArr xs
