{-# LANGUAGE TupleSections #-}

module Days.Fourteen (
    solve,
) where

import Data.List.Split (divvy, splitOn)
import Data.Map as Map (Map, elems, fromList, insert, keys, lookup)

solve :: Int -> String -> String
solve 1 = show . length . filter (== Sand) . elems . (\ grd -> simulateSand (getBottom grd) (500, 0) grd) . parseLines
solve 2 = show . length . filter (== Sand) . elems . (\ grd -> simulateSandFlr (getBottom grd) (500, 0) grd) . insertFloor 500 . parseLines
solve _ = const "Invalid part"

type Pos = (Int, Int)

data Tile
    = Rock
    | Sand
    deriving (Show, Eq)

parseLines :: String -> Map Pos Tile
parseLines = fromList . map (, Rock) . concatMap parseLine . lines

parseLine :: String -> [Pos]
parseLine = concatMap (\ [l, r] -> posRange l r) . divvy 2 1 . map parsePos . splitOn " -> "

parsePos :: String -> Pos
parsePos = (\ [x, y] -> (read x, read y)) . splitOn ","

posRange :: Pos -> Pos -> [Pos]
posRange (xl, yl) (xr, yr)
    | xl == xr = map (xl, ) [min yl yr..max yl yr]
    | yl == yr = map (, yl) [min xl xr..max xl xr]
    | otherwise = []

getNextPos :: Pos -> Map Pos Tile -> Maybe Pos
getNextPos (x, y) grd = case (Map.lookup (x, y + 1) grd, Map.lookup (x - 1, y + 1) grd, Map.lookup (x + 1, y + 1) grd) of
    (Nothing, _, _) -> Just (x, y + 1)
    (_, Nothing, _) -> Just (x - 1, y + 1)
    (_, _, Nothing) -> Just (x + 1, y + 1)
    _ -> Nothing

getBottom :: Map Pos Tile -> Int
getBottom = maximum . map snd . keys

simulateSand :: Int -> Pos -> Map Pos Tile -> Map Pos Tile
simulateSand btm src grd = let
    (fin, drp) = dropSand btm src grd
    in if fin then drp else simulateSand btm src drp

dropSand :: Int -> Pos -> Map Pos Tile -> (Bool, Map Pos Tile)
dropSand btm frm grd = let
    to = getNextPos frm grd
    in case to of
        Nothing -> (False, insert frm Sand grd)
        Just to @ (_, y) -> if y >= btm then (True, grd) else dropSand btm to grd

simulateSandFlr :: Int -> Pos -> Map Pos Tile -> Map Pos Tile
simulateSandFlr btm src grd = let
    (fin, drp) = dropSandFlr btm src grd
    in if fin || Map.lookup src grd == Just Sand then drp else simulateSandFlr btm src drp

dropSandFlr :: Int -> Pos -> Map Pos Tile -> (Bool, Map Pos Tile)
dropSandFlr btm frm grd = let
    to = getNextPos frm grd
    in case to of
        Nothing -> (False, insert frm Sand grd)
        Just to @ (_, y) -> if y >= btm then (True, insert frm Sand grd) else dropSandFlr btm to grd

insertFloor :: Int -> Map Pos Tile -> Map Pos Tile
insertFloor x grd = let
    b = getBottom grd + 2
    pts = posRange (x - (2 * b) - 1, b) (x + (2 * b) + 1, b)
    in foldr (`insert` Rock) grd pts