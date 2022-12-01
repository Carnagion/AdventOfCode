module Days.One (
    solve,
) where

import Data.List (sort)
import Data.List.Split (splitOn)

solve :: Int -> String -> String
solve 1 = show . maximum . parseCalories . filterCalories . splitCalories
solve 2 = show . sum . take 3 . reverse . sort . parseCalories . filterCalories . splitCalories
solve _ = const "Invalid part"

splitCalories :: String -> [[String]]
splitCalories = map (splitOn "\n") . splitOn "\n\n"

filterCalories :: [[String]] -> [[String]]
filterCalories = filter (not . null) . map (filter (not . null))

parseCalories :: [[String]] -> [Int]
parseCalories = map (sum . map read)