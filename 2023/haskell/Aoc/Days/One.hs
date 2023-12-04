module Aoc.Days.One (
    partOne,
    partTwo,
) where

import Data.Char (isDigit)

import Text.Read (readMaybe)

import Aoc (Input)

partOne :: Input -> Int
partOne = sum . map (read . edges . filter isDigit) . lines

partTwo :: Input -> Int
partTwo = sum . map (concatDigits . edges . nums) . lines

edges :: [a] -> [a]
edges = edgesWith []
  where
    edgesWith :: [a] -> [a] -> [a]
    edgesWith _ [] = []
    edgesWith [] [x] = [x, x]
    edgesWith [x] [y] = [x, y]
    edgesWith [] (x : xs) = edgesWith [x] xs
    edgesWith acc (x : xs) = edgesWith acc xs

nums :: String -> [Int]
nums ('o' : 'n' : 'e' : rest) = 1 : nums ('e' : rest)
nums ('t' : 'w' : 'o' : rest) = 2 : nums ('o' : rest)
nums ('t' : 'h' : 'r' : 'e' : 'e' : rest) = 3 : nums ('e' : rest)
nums ('f' : 'o' : 'u' : 'r' : rest) = 4 : nums rest
nums ('f' : 'i' : 'v' : 'e' : rest) = 5 : nums ('e' : rest)
nums ('s' : 'i' : 'x' : rest) = 6 : nums rest
nums ('s' : 'e' : 'v' : 'e' : 'n' : rest) = 7 : nums ('n' : rest)
nums ('e' : 'i' : 'g' : 'h' : 't' : rest) = 8 : nums ('t' : rest)
nums ('n' : 'i' : 'n' : 'e' : rest) = 9 : nums ('e' : rest)
nums (c : rest) = case readMaybe [c] of
    Just d -> d : nums rest
    Nothing -> nums rest
nums "" = []

concatDigits :: [Int] -> Int
concatDigits [tens, ones] = tens * 10 + ones
concatDigits _ = 0

readNextNum :: String -> (Maybe Int, String)
readNextNum ('o' : 'n' : 'e' : cs) = (Just 1, cs)
readNextNum ('t' : 'w' : 'o' : cs) = (Just 2, cs)
readNextNum ('t' : 'h' : 'r' : 'e' : 'e' : cs) = (Just 3, cs)
readNextNum ('f' : 'o' : 'u' : 'r' : cs) = (Just 4, cs)
readNextNum ('f' : 'i' : 'v' : 'e' : cs) = (Just 5, cs)
readNextNum ('s' : 'i' : 'x' : cs) = (Just 6, cs)
readNextNum ('s' : 'e' : 'v' : 'e' : 'n' : cs) = (Just 7, cs)
readNextNum ('e' : 'i' : 'g' : 'h' : 't' : cs) = (Just 8, cs)
readNextNum ('n' : 'i' : 'n' : 'e' : cs) = (Just 9, cs)
readNextNum str = (Nothing, str)