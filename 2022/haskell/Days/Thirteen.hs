module Days.Thirteen (
    solve,
) where

import Data.Char (isDigit)
import Data.Maybe (fromJust)
import Data.List (findIndices, sort)
import Data.List.Split (chunksOf)
import Data.Void (Void)

import Text.Megaparsec (Parsec, (<|>), between, parseMaybe, sepBy, single, takeWhile1P)

solve :: Int -> String -> String
solve 1 = show . sum . map fst . filter ((/= GT) . snd) . zipWith (\ idx [l, r] -> (idx, compare l r)) [1..] . chunksOf 2 . map (fromJust . parseMaybe packet) . filter (not . null) . lines
solve 2 = show . product . map (+ 1) . findIndices (`elem` dividerPckts) . sort . (++ dividerPckts) . map (fromJust . parseMaybe packet) . filter (not . null) . lines
solve _ = const "Invalid part"

data Packet
    = Number Int
    | List [Packet]
    deriving (Show, Eq)

instance Ord Packet where
    compare (Number l) (Number r) = compare l r
    compare lst @ (List _) num @ (Number _) = compare lst (List [num])
    compare num @ (Number _) lst @ (List _) = compare (List [num]) lst
    compare (List []) (List []) = EQ
    compare (List []) (List _) = LT
    compare (List _) (List []) = GT
    compare (List (lhd : ltl)) (List (rhd : rtl)) = case compare lhd rhd of
        EQ -> compare (List ltl) (List rtl)
        ord -> ord

type Parser = Parsec Void String

packet :: Parser Packet
packet = numberPckt <|> listPckt

numberPckt :: Parser Packet
numberPckt = Number . read <$> takeWhile1P (Just "number") isDigit

listPckt :: Parser Packet
listPckt = List <$> between (single '[') (single ']') (sepBy packet (single ','))

dividerPckts :: [Packet]
dividerPckts = [List [List [Number 2]], List [List [Number 6]]]