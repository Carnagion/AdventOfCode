module Days.Three (
    solve,
) where

import Data.Char (isUpper, ord)
import Data.List (find)
import Data.List.Split (chunksOf)
import Data.Maybe (fromJust)

solve :: Int -> String -> String
solve 1 = show . sum . map (getPriority . findCommonItem) . splitCompartments . splitRucksacks
solve 2 = show . sum . map (getPriority . findBadge) . chunksOf 3 . splitRucksacks
solve _ = const "Invalid part"

splitRucksacks :: String -> [String]
splitRucksacks = filter (not . null) . lines

splitCompartments :: [String] -> [(String, String)]
splitCompartments = map (splitAt . (`div` 2) =<< length)

findCommonItem :: (String, String) -> Char
findCommonItem (l, r) = fromJust $ find (`elem` r) l

getPriority :: Char -> Int
getPriority c = ord c - (if isUpper c then 38 else 96)

findBadge :: [String] -> Char
findBadge rs = fromJust $ find (\ c -> all (elem c) rs) (['a'..'z'] ++ ['A'..'Z'])