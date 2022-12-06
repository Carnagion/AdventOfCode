module Main (
    main,
) where

import System.Environment (getArgs)

import Days.One (solve)
import Days.Two (solve)
import Days.Three (solve)
import Days.Four (solve)
import Days.Five (solve)
import Days.Six (solve)

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
getSolution 1 = Days.One.solve
getSolution 2 = Days.Two.solve
getSolution 3 = Days.Three.solve
getSolution 4 = Days.Four.solve
getSolution 5 = Days.Five.solve
getSolution 6 = Days.Six.solve
getSolution _ = const $ const "Invalid day"