module Days.Six (
    solve,
) where

import Data.List (nub)
import Data.List.Split (divvy)

solve :: Int -> String -> String
solve 1 = show . findPosition 4
solve 2 = show . findPosition 14
solve _ = const "Invalid part"

findPosition :: Int -> String -> Int
findPosition n = last . map fst . head . filter (\ win -> map snd win == nub (map snd win)) . divvy n 1 . zip [1..]