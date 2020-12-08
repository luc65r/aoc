import Text.Read

data Instruction = Acc Int | Jmp Int | Nop Int
    deriving (Show)

instance Read Instruction where
    readPrec = do
        Ident op <- lexP
        Symbol sign <- lexP
        n <- step readPrec
        let arg = case sign of
                    "+" -> n
                    "-" -> -n
        return $ case op of
                   "acc" -> Acc arg
                   "jmp" -> Jmp arg
                   "nop" -> Nop arg

data Program = Program { instructions :: [(Instruction, Bool)]
                       , place :: Int
                       , accumulator :: Int
                       , infinite :: Bool
                       , ended :: Bool
                       }

main = do input <- lines <$> getContents
          let ins = map read input
          let prog = Program (zip ins (repeat False)) 0 0 False False
          putStrLn . show . accumulator . execute $ prog
          putStrLn . show . accumulator . head . filter ended . map (\i -> execute $ Program (zip i (repeat False)) 0 0 False False) . changed $ ins

changed :: [Instruction] -> [[Instruction]]
changed [] = [[]]
changed (i:is) = case i of
                   Acc n -> map (i:) . changed $ is
                   Jmp n -> ((Nop n):is) : (map (i:) . changed $ is)
                   Nop n -> ((Jmp n):is) : (map (i:) . changed $ is)

execute :: Program -> Program
execute = until (\p -> infinite p || ended p) oneStep

oneStep :: Program -> Program
oneStep (Program ins pl acc False False)
    | pl == length ins = Program ins pl acc False True
    | otherwise = 
        case ins !! pl of
          (_, True) -> Program ins pl acc True False
          (Acc n, _) -> Program (replace pl (Acc n, True) ins) (pl + 1) (acc + n) False False
          (Jmp n, _) -> Program (replace pl (Jmp n, True) ins) (pl + n) acc False False
          (Nop n, _) -> Program (replace pl (Nop n, True) ins) (pl + 1) acc False False

replace :: Int -> a -> [a] -> [a]
replace n x l = xs ++ x:ys
    where (xs, _:ys) = splitAt n l
