main = interact countTrees

countTrees :: String -> String
countTrees = (++ "\n") . show . trees . map (concat . repeat . parseLine) . lines

parseLine :: String -> [Bool]
parseLine = map (== '#')

trees :: [[Bool]] -> Int
trees [(x:_)] = if x then 1 else 0
trees ((x:_):ys) = (if x then 1 else 0) + trees (map (drop 3) ys)
