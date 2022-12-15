module Days.Fifteen (
    solve,
) where

import Data.List.Split (splitOn)
import Data.Range (Bound (..), BoundType (..), Range (..), (+=+), difference, mergeRanges)

solve :: Int -> String -> String
solve 1 = show . mergeRanges . concatMap (yCoverage 2000000 . parseSensor) . lines
solve 2 = show . filter (not . null . snd) . yRangeCoverages 0 4000000 . map parseSensor . lines
solve _ = const "Invalid part"

type Pos = (Int, Int)

data Sensor = Sensor Pos Pos
    deriving (Show)

parsePos :: String -> Pos
parsePos = (\ [x, y] -> (read x, read y)) . splitOn "," . filter (`elem` "0123456789-+,")

parseSensor :: String -> Sensor
parseSensor = (\ [sns, bcn] -> Sensor (parsePos sns) (parsePos bcn)) . splitOn ":"

manhattan :: Pos -> Pos -> Int
manhattan (xl, yl) (xr, yr) = abs (xr - xl) + abs (yr - yl)

-- yCoverage :: Int -> Sensor -> Set Int
-- yCoverage y (Sensor sns @ (snsx, snsy) bcn @ (bcnx, bcny)) = let
--     rad = manhattan sns bcn
--     dif = abs (y - snsy)
--     rem = rad - dif
--     rng = fromDistinctAscList (if rem <= 0 then [] else [snsx - rem .. snsx + rem])
--     in if bcny == y then delete bcnx rng else rng

yCoverage :: Int -> Sensor -> [Range Int]
yCoverage y (Sensor sns @ (snsx, snsy) bcn @ (bcnx, bcny)) = let
    rad = manhattan sns bcn
    dif = abs (y - snsy)
    rem = rad - dif
    rng = [(snsx - rem) +=+ (snsx + rem) | rem > 0]
    in if bcny == y then difference rng [SingletonRange bcnx] else rng

yRangeCoverages :: Int -> Int -> [Sensor] -> [(Int, [Range Int])]
yRangeCoverages frm to snsrs = map (\ y -> (y, filter missingExcl (difference [frm +=+ to] (yCoverages y snsrs)))) [frm .. to]

yCoverages :: Int -> [Sensor] -> [Range Int]
yCoverages y = mergeRanges . concatMap (yCoverage y)

missingExcl :: Range Int -> Bool
missingExcl (SpanRange (Bound l Exclusive) (Bound r Exclusive))
    | r - l == 2 = True
    | otherwise = False
missingExcl _ = False