module Main (
    main,
) where

import System.Environment (getArgs)

import Aoc (Day, Input, Part)
import Aoc.Days.One
import Aoc.Days.Two

main :: IO ()
main = do
    args <- getArgs
    case args of
        [dayStr, partStr, file] -> do
            inp <- readFile file
            let day = read dayStr
            let part = read partStr
            putStrLn $ solve day part inp
        _ -> error "Invalid usage"

solve :: Int -> Int -> Input -> String
solve 1 part = choosePart part Aoc.Days.One.partOne Aoc.Days.One.partTwo
solve 2 part = choosePart part Aoc.Days.Two.partOne Aoc.Days.Two.partTwo
solve _ _ = error "Invalid day"

choosePart :: (Show a, Show b) => Int -> (Input -> a) -> (Input -> b) -> (Input -> String)
choosePart 1 one _ = show . one
choosePart 2 _ two = show . two
choosePart _ _ _ = error "Invalid part"
