module Days.Eleven (
    solve,
) where

import Data.Bifunctor (bimap)
import Data.Char (isDigit, isSpace)
import Data.List (sort)
import Data.List.Split (chunksOf, splitOn)

solve :: Int -> String -> String
solve 1 = show . product . take 2 . reverse . sort . map times . performRounds False 20 . parseMonkeys
solve 2 = show . product . take 2 . reverse . sort . map times . performRounds True 10000 . parseMonkeys
solve _ = const "Invalid part"

data Monkey = Monkey { items :: [Int], operation :: Int -> Int, test :: Int, outcomes :: (Int, Int), times :: Int }

parseMonkeys :: String -> [Monkey]
parseMonkeys = map parseMonkey . chunksOf 7 . lines

parseMonkey :: [String] -> Monkey
parseMonkey lns = let
    itms = map read (splitOn "," (splitOn ":" (filter (not . isSpace) (lns !! 1)) !! 1))
    op = parseOp (splitOn "= " (lns !! 2) !! 1)
    tst = read (filter isDigit (lns !! 3))
    outc = bimap (read . filter isDigit) (read . filter isDigit) (lns !! 4, lns !! 5)
    in Monkey itms op tst outc 0

parseOp :: String -> Int -> Int
parseOp ln = case words ln of
    ["old", "*", "old"] -> (\ old -> old * old)
    ["old", "*", num] -> (* read num)
    ["old", "+", num] -> (+ read num)
    _ -> id

performRounds :: Bool -> Int -> [Monkey] -> [Monkey]
performRounds p2 0 = id
performRounds p2 r = performRounds p2 (r - 1) . performTurns p2 0

performTurns :: Bool -> Int -> [Monkey] -> [Monkey]
performTurns p2 n mnks
    | n >= length mnks = mnks
    | null (items (mnks !! n)) = performTurns p2 (n + 1) mnks
    | otherwise = performTurns p2 (n + 1) (performTurn p2 n mnks)

-- This works on the principle that n mod m == n mod (m * k) where k is an integer
-- and (a + b) mod m == (a mod m) + (b mod m)
-- and similarly (a * b) mod m == (a mod m) * (b mod m)
-- I'm sorry
performTurn :: Bool -> Int -> [Monkey] -> [Monkey]
performTurn p2 frm mnks
    | null (items (mnks !! frm)) = mnks
    | otherwise = let
        (Monkey (itm : itms) op tst (t, f) tms) = mnks !! frm
        wor = if p2 then op itm else op itm `div` 3
        to = if wor `mod` tst == 0 then t else f
        wor' = if p2 then wor `mod` foldr1 lcm (map test mnks) else wor
        (Monkey itms' op' tst' outc' tms') = mnks !! to
        catcher = Monkey (itms' ++ [wor']) op' tst' outc' tms'
        thrower = Monkey itms op tst (t, f) (tms + 1)
        in performTurn p2 frm (setAt frm thrower (setAt to catcher mnks))

setAt :: Int -> a -> [a] -> [a]
setAt at rep = setAt' at rep . zip [0..]
    where
        setAt' _ _ [] = []
        setAt' at rep ((idx, x) : xs)
            | idx == at = rep : map snd xs
            | otherwise = x : setAt' at rep xs