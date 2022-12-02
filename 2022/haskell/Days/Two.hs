module Days.Two (
    solve,
) where

import Data.Maybe (fromJust)

solve :: Int -> String -> String
solve 1 = show . sum . map (uncurry getWinningScore) . splitMoves
solve 2 = show . sum . map (uncurry getStrategyScore) . splitMoves
solve _ = const "Invalid part"

splitMoves :: String -> [(Int, Int)]
splitMoves = map (\ str -> (moveValue $ head str, moveValue $ str !! 2)) . filter (not . null) . lines

moveValue :: Char -> Int
moveValue = fromJust . (`lookup` zip "AXBYCZ" [0, 0, 1, 1, 2, 2])

getWinningScore :: Int -> Int -> Int
getWinningScore opp self = self + 1 + (((((self - opp) `mod` 3) + 1) `mod` 3) * 3)

getStrategyScore :: Int -> Int -> Int
getStrategyScore opp self = (self * 3) + ((opp + (self - 1)) `mod` 3) + 1