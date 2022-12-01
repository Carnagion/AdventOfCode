module Days.One (
    partOne,
    partTwo
) where

import Data.List (sort)
import Data.List.Split (splitOn)

partOne :: String -> String
partOne = show . maximum . parseCalories . filterCalories . splitCalories

partTwo :: String -> String
partTwo = show . sum . take 3 . reverse . sort . parseCalories . filterCalories . splitCalories

splitCalories :: String -> [[String]]
splitCalories inp = map (splitOn "\n") (splitOn "\n\n" inp)

filterCalories :: [[String]] -> [[String]]
filterCalories = filter (not . null) . map (filter (not . null))

parseCalories :: [[String]] -> [Int]
parseCalories = map (sum . map read)