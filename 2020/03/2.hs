main = interact countTrees

countTrees :: String -> String
countTrees s = let slope = map (concat . repeat . parseLine) . lines $ s
                   ways = [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]
                in (++ "\n") . show . product . map (flip trees slope) $ ways

parseLine :: String -> [Bool]
parseLine = map (== '#')

trees :: (Int, Int) -> [[Bool]] -> Int
trees _ [(x:_)] = if x then 1 else 0
trees dir@(right, down) xs@((x:_):_) = (if x then 1 else 0) +
    trees dir (map (drop right) . drop down $ xs)
