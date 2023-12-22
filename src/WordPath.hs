module WordPath
    ( WordPath(..)
    ) where

data WordPath = WordPath { word :: String
                         , path :: [(Int, Int)] }

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
