main = do slope <- (map (cycle . parseLine) . lines) <$> getContents
          putStrLn . show . trees (3, 1) $ slope
          let ways = [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]
          putStrLn . show . product . map (flip trees slope) $ ways

parseLine :: String -> [Bool]
parseLine = map (== '#')

trees :: (Int, Int) -> [[Bool]] -> Int
trees _ [(x:_)] = bto x
trees dir@(right, down) xs@((x:_):_) = bto x + trees dir ahead
    where ahead = map (drop right) . drop down $ xs

bto :: Bool -> Int
bto False = 0
bto True = 1
