module Days.Seven (
    solve,
) where

import Data.Char (isSpace)
import Data.List.Split (splitWhen)

solve :: Int -> String -> String
solve 1 = show . sum . filter (<= 100000) . getDirSizes . fst . parseFs (Dir []) . lines
solve 2 = show . findDeletable . fst . parseFs (Dir []) . lines
solve _ = const "Invalid part"

data Fs
    = File Int
    | Dir [Fs]
    deriving (Show)

size :: Fs -> Int
size (File s) = s
size (Dir fs) = sum $ map size fs

subfs :: Fs -> [Fs]
subfs (Dir fs) = fs
subfs _ = []

parseFs :: Fs -> [String] -> (Fs, [String])
parseFs fs [] = (fs, [])
parseFs fs (ln : lns) = case words ln of
    ["$", "cd", ".."] -> (fs, lns)
    ["$", "cd", "/"] -> parseFs fs lns
    ["$", "cd", _] -> let (sub, rem) = parseFs (Dir []) lns
        in parseFs (Dir (sub : subfs fs)) rem
    ["$", "ls"] -> parseFs fs lns
    ["dir", _] -> parseFs fs lns
    [num, _] -> parseFs (Dir (File (read num) : subfs fs)) lns
    _ -> (fs, lns)

getDirSizes :: Fs -> [Int]
getDirSizes dir @ (Dir subfs) = size dir : concatMap getDirSizes subfs
getDirSizes _ = []

findDeletable :: Fs -> Int
findDeletable fs = let req = 30000000 - (70000000 - size fs)
    in minimum . filter (>= req) . getDirSizes $ fs