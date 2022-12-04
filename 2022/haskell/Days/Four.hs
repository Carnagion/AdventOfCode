module Days.Four (
    solve,
) where

import Data.List.Split (splitOn)

solve :: Int -> String -> String
solve 1 = show . length . filter (uncurry eitherContains) . parseAssignments . splitAssignments
solve 2 = show . length . filter (uncurry overlaps) . parseAssignments . splitAssignments
solve _ = const "Invalid part"

data Range = Range { lower :: Int, upper :: Int }
    deriving (Show)

contains :: Range -> Range -> Bool
l `contains` r = lower l <= lower r && upper l >= upper r

eitherContains :: Range -> Range -> Bool
eitherContains l r = l `contains` r || r `contains` l

overlaps :: Range -> Range -> Bool
l `overlaps` r = upper l >= lower r && lower l <= upper r

parseRange :: String -> Range
parseRange str = let [lower, upper] = splitOn "-" str
    in Range (read lower) (read upper)

splitAssignments :: String -> [[String]]
splitAssignments = map (splitOn ",") . filter (not . null) . lines

parseAssignments :: [[String]] -> [(Range, Range)]
parseAssignments = map (\ strs -> (parseRange $ head strs, parseRange $ strs !! 1))