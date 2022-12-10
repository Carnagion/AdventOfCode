module Days.Nine (
    solve,
) where

import Data.List (nub)

solve :: Int -> String -> String
solve 1 = show . length . nub . visited . foldl moveRope (Rope (replicate 2 (0, 0)) []) . parseMoves
solve 2 = show . length . nub . visited . foldl moveRope (Rope (replicate 10 (0, 0)) []) . parseMoves
solve _ = const "Invalid part"

type Pos = (Int, Int)

data Direction
    = R
    | L
    | U
    | D
    deriving (Show, Read)

data Move = Move { direction :: Direction, amount :: Int }
    deriving (Show)

data Rope = Rope { knots :: [Pos], visited :: [Pos] }
    deriving (Show)

parseMoves :: String -> [Move]
parseMoves = map ((\ [dir, amt] -> Move (read dir) (read amt)) . words) . lines

directionPos :: Direction -> Pos
directionPos R = (1, 0)
directionPos L = (-1, 0)
directionPos U = (0, 1)
directionPos D = (0, -1)

addPos :: Pos -> Pos -> Pos
(xl, yl) `addPos` (xr, yr) = (xl + xr, yl + yr)

subPos :: Pos -> Pos -> Pos
(xl, yl) `subPos` (xr, yr) = (xl - xr, yl - yr)

moveRope :: Rope -> Move -> Rope
moveRope rop (Move _ 0) = rop
moveRope (Rope knts vis) (Move dir amt) = let
    rop = movePair dir [] knts
    in moveRope (Rope rop (last rop : vis)) (Move dir (amt - 1))

moveKnot :: Pos -> Pos
moveKnot (x, y)
    | abs x <= 1 && abs y <= 1 = (0, 0)
    | otherwise = (signum x, signum y)

movePair :: Direction -> [Pos] -> [Pos] -> [Pos]
movePair dir pos [] = reverse pos
movePair dir [] (hd : tl) = movePair dir [hd `addPos` directionPos dir] tl
movePair dir mov @ (prev : _) (cur : rem) = let
    knt = moveKnot (prev `subPos` cur) `addPos` cur
    in movePair dir (knt : mov) rem