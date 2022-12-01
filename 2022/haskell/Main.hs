module Main (
    main,
) where

import System.Environment (getArgs)

import Days.One

main :: IO ()
main = do
    args <- getArgs
    let day = read $ head args
    let part = read $ args !! 1
    input <- getPuzzleInput day
    print $ getSolution day part input

getPuzzleInput :: Int -> IO String
getPuzzleInput day = readFile $ "Inputs/" ++ show day ++ ".txt"

getSolution :: Int -> Int -> String -> String
getSolution 1 1 inp = Days.One.partOne inp
getSolution 1 2 inp = Days.One.partTwo inp
getSolution _ _ _ = "Invalid day or part"