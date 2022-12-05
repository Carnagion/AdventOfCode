module Days.Five (
    solve,
) where

import Data.Bifunctor (bimap)
import Data.Char (isDigit, isSpace)
import Data.List (transpose)
import Data.List.Split (splitOn)

solve :: Int -> String -> String
solve 1 = map head . uncurry (flip (moveCrates True)) . bimap parseCrates parseInstructions . splitInput
solve 2 = map head . uncurry (flip (moveCrates False)) . bimap parseCrates parseInstructions . splitInput
solve _ = const "Invalid part"

data MoveInstr = Move { amount :: Int, from :: Int, to :: Int }
    deriving (Show)

splitInput :: String -> (String, String)
splitInput = (\ [crt, instr] -> (crt, instr)) . splitOn "\n\n"

parseCrates :: String -> [String]
parseCrates = filter (not . null) . map (filter (not . (`elem` "[ ]"))) . transpose . init . lines

parseInstructions :: String -> [MoveInstr]
parseInstructions = map (parseInstr . tail . filter (\ c -> isDigit c || isSpace c)) . lines

parseInstr :: String -> MoveInstr
parseInstr = (\ [amt, frm, to] -> Move (read amt) (read frm - 1) (read to - 1)) . splitOn "  "

moveCrates :: Bool -> [MoveInstr] -> [String] -> [String]
moveCrates rev instrs crts = foldl (flip (moveCrate rev)) crts instrs

moveCrate :: Bool -> MoveInstr -> [String] -> [String]
moveCrate rev (Move amt frm to) crts = let
    frmCrt = crts !! frm
    toCrt = crts !! to
    (mov, rem) = splitAt amt frmCrt
    in setAt frm rem (setAt to ((if rev then reverse mov else mov) ++ toCrt) crts)

setAt :: Int -> a -> [a] -> [a]
setAt at rep = setAt' at rep . zip [0..]
    where
        setAt' _ _ [] = []
        setAt' at rep ((idx, x) : xs)
            | idx == at = rep : map snd xs
            | otherwise = x : setAt' at rep xs