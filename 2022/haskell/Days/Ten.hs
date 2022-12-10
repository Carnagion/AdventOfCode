module Days.Ten (
    solve,
) where

solve :: Int -> String -> String
solve 1 = show . strength . advanceCycle (Prog 0 0 1 0 "") . parseInstrs
solve 2 = reverse . display . advanceCycle (Prog 0 0 1 0 "") . parseInstrs
solve _ = const "Invalid part"

data Instr
    = NoOp
    | AddX Int
    deriving (Show)

data Prog = Prog { cycles :: Int, time :: Int, register :: Int, strength :: Int, display :: String }
    deriving (Show)

advanceCycle :: Prog -> [Instr] -> Prog
advanceCycle prog [] = prog
advanceCycle (Prog cyc 1 reg str disp) (NoOp : instrs) = advanceCycle (Prog cyc 0 reg str disp) instrs
advanceCycle (Prog cyc 2 reg str disp) (AddX x : instrs) = advanceCycle (Prog cyc 0 (reg + x) str disp) instrs
advanceCycle (Prog cyc tm reg str disp) instrs = let
    in advanceCycle (drawSprite (checkStrength (Prog (cyc + 1) (tm + 1) reg str disp))) instrs

checkStrength :: Prog -> Prog
checkStrength prog @ (Prog cyc tm reg str disp)
    | cyc `elem` [20, 60, 100, 140, 180, 220] = Prog cyc tm reg (str + cyc * reg) disp
    | otherwise = prog

drawSprite :: Prog -> Prog
drawSprite (Prog cyc tm reg str disp) = let
    px = (cyc - 1) `mod` 40
    spr = if abs (reg - px) <= 1 then '#' else '.'
    drawn = if px == 39 then '\n' : spr : disp else spr : disp
    in Prog cyc tm reg str drawn

parseInstrs :: String -> [Instr]
parseInstrs = map parseInstr . lines

parseInstr :: String -> Instr
parseInstr "noop" = NoOp
parseInstr str = case words str of
    ["addx", num] -> AddX (read num)
    _ -> NoOp