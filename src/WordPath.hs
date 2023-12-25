module WordPath
    ( WordPath(..)
    , Direction(..)
    , dirPosPairs
    , dirChar
    , offset
    , opposite
    ) where
import Control.Applicative (Applicative(liftA2))

data WordPath = WordPath { word :: String
                         , path :: [(Int, Int)] }

data Direction = N | NE | E | SE | S | SW | W | NW | X

direction :: (Int, Int) -> Direction
direction x = case x of
    (-1, 0)  -> N
    (-1, 1)  -> NE
    (0, 1)   -> E
    (1, 1)   -> SE
    (1, 0)   -> S
    (1, -1)  -> SW
    (0, -1)  -> W
    (-1, -1) -> NW
    _        -> X

offset :: Direction -> (Int, Int)
offset x = case x of
    N  -> (-1, 0)
    NE -> (-1, 1)
    E  -> (0, 1)
    SE -> (1, 1)
    S  -> (1, 0)
    SW -> (1, -1)
    W  -> (0, -1)
    NW -> (-1, -1)
    _  -> (0, 0)

dirChar :: Direction -> Char
dirChar x = case x of
    N  -> '↑'
    NE -> '↗'
    E  -> '→'
    SE -> '↘'
    S  -> '↓'
    SW -> '↙'
    W  -> '←'
    NW -> '↖'
    X  -> '·'

opposite :: Direction -> Direction
opposite x = case x of
    N  -> S
    NE -> SW
    E  -> W
    SE -> NW
    S  -> N
    SW -> NE
    W  -> E
    NW -> SE
    X  -> X

instance Show Direction where
    show = show . dirChar

directions :: WordPath -> [Direction]
directions (WordPath _ p) = X : zipWith f p (drop 1 p)
    where f (a, b) (c, d) = direction (c-a, d-b)

dirPosPairs :: WordPath -> [((Int, Int), Direction)]
dirPosPairs = liftA2 zip path directions
