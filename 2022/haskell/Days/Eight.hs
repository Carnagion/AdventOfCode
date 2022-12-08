module Days.Eight (
    solve,
) where

import Data.Bifunctor (bimap)

solve :: Int -> String -> String
solve 1 = show . length . (\ g -> concatMap (filter (isVisible g)) g) . withPos . parseGrid
solve 2 = show . maximum . (\ g -> concatMap (map (scenicScore g)) g) . withPos . parseGrid
solve _ = const "Invalid part"

parseGrid :: String -> [[Int]]
parseGrid = map (map (read . (: []))) . lines

withPos :: [[a]] -> [[(a, Int, Int)]]
withPos = zipWith (\ r -> zipWith (\ c x -> (x, r, c)) [0 .. ]) [0..]

isVisible :: [[(Int, Int, Int)]] -> (Int, Int, Int) -> Bool
isVisible g (x, r, c)
    | r == 0 || c == 0 = True
    | r + 1 == length g || c + 1 == length (head g) = True
    | otherwise = let
        (befH, aftH) = splitAt c (g !! r)
        (befV, aftV) = splitAt r (map (!! c) g)
        cond = (< x) . fstt
        in all cond befH || all cond (tail aftH) || all cond befV || all cond (tail aftV)

scenicScore :: [[(Int, Int, Int)]] -> (Int, Int, Int) -> Int
scenicScore g (x, r, c) = let
    bmap = bimap (map fstt . reverse) (map fstt . tail)
    (befH, aftH) = bmap (splitAt c (g !! r))
    (befV, aftV) = bmap (splitAt r (map (!! c) g))
    in length (takeUnblocking x befH) * length (takeUnblocking x aftH) * length (takeUnblocking x befV) * length (takeUnblocking x aftV)

fstt :: (a, b, c) -> a
fstt (a, _, _) = a

takeUnblocking :: Int -> [Int] -> [Int]
takeUnblocking _ [] = []
takeUnblocking x (hd : tl)
    | hd >= x = [hd]
    | otherwise = hd : takeUnblocking x tl