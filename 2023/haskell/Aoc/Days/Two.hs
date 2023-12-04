module Aoc.Days.Two (
    partOne,
    partTwo,
) where

import Data.Char (isDigit, isSpace)
import Data.List (span)
import Data.List.Split (splitOn)

import Aoc (Input)

partOne :: Input -> Int
partOne = sum . map gameId . filter (all (\(Reveal r g b) -> r <= 12 && g <= 13 && b <= 14) . reveals) . map parseGame . lines

partTwo :: Input -> Int
partTwo = sum . map (power . minCubeSet . reveals . parseGame) . lines

data Game = Game {gameId :: Int, reveals :: [Reveal]}
    deriving (Show)

data Reveal = Reveal {red :: Int, green :: Int, blue :: Int}
    deriving (Show)

parseGame :: String -> Game
parseGame str =
    let str' = drop 5 str
        (idStr, ':' : ' ' : str'') = span isDigit str'
        id = read idStr
        rvls = map (addColours . map parseColour . splitOn ",") $ splitOn ";" str''
     in Game id rvls

parseColour :: String -> (Int, Int, Int)
parseColour str =
    let (amtStr, ' ' : clr) = span isDigit (dropWhile isSpace str)
        amt = read amtStr
     in case clr of
            "red" -> (amt, 0, 0)
            "green" -> (0, amt, 0)
            "blue" -> (0, 0, amt)
            _ -> error "Invalid colour"

addColours :: [(Int, Int, Int)] -> Reveal
addColours = foldr (\(r, g, b) (Reveal r' g' b') -> Reveal (r + r') (g + g') (b + b')) (Reveal 0 0 0)

minCubeSet :: [Reveal] -> Reveal
minCubeSet = foldr (\(Reveal lr lg lb) (Reveal rr rg rb) -> Reveal (max lr rr) (max lg rg) (max lb rb)) (Reveal 0 0 0)

power :: Reveal -> Int
power (Reveal r g b) = r * g * b