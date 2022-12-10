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
import Days.Seven (solve)
import Days.Eight (solve)
import Days.Nine (solve)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [dayStr, partStr, file] -> do
            inp <- readFile file
            let day = read dayStr
            let part = read partStr
            print $ getSolution day part inp
        _ -> print "Invalid usage"

getPuzzleInput :: Int -> IO String
getPuzzleInput day = readFile $ "Inputs/" ++ show day ++ ".txt"

getSolution :: Int -> Int -> String -> String
getSolution 1 = Days.One.solve
getSolution 2 = Days.Two.solve
getSolution 3 = Days.Three.solve
getSolution 4 = Days.Four.solve
getSolution 5 = Days.Five.solve
getSolution 6 = Days.Six.solve
getSolution 7 = Days.Seven.solve
getSolution 8 = Days.Eight.solve
getSolution 9 = Days.Nine.solve
getSolution _ = const $ const "Invalid day"