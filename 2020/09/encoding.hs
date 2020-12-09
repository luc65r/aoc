main = do input <- (map read . lines) <$> getContents
          let ns = firstNonSum $ input
          putStrLn . show $ ns
          putStrLn . show . findC ns $ input

firstNonSum :: [Int] -> Int
firstNonSum xs
    | isSum x (take 25 xs) = firstNonSum $ tail xs
    | otherwise = x
    where x = xs !! 25

isSum :: Int -> [Int] -> Bool
isSum n = any ((== n) . uncurry (+)) . pairs

findC :: Int -> [Int] -> Int
findC n xs
    | last s == n = minimum range + maximum range
    | otherwise = findC n (tail xs)
    where s = takeWhile (<= n) . scanl (+) 0 $ xs
          range = take (length s - 1) xs

pairs :: [a] -> [(a, a)]
pairs [x] = []
pairs (x:xs) = map ((,) x) xs ++ pairs xs
