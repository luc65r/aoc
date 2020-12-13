import Data.List
import Text.Read

main = do ts <- read <$> getLine
          ids <- (map readMaybe . split ',') <$> getLine
          putStrLn . show . uncurry (*)
            . minimumBy (\(_, x) (_, y) -> compare x y)
            . map (\x -> (x, x - (ts `mod` x)))
            . map (\(Just n) -> n)
            . filter (/= Nothing) $ ids

          let Just fid = head ids
              Just time = find (\n -> follow n ids) . iterate (+ fid) $ fid
          putStrLn . show $ time

follow :: Integer -> [Maybe Integer] -> Bool
follow n [Nothing] = True
follow n [(Just x)] = n `mod` x == 0
follow n (Nothing:xs) = follow (n + 1) xs
follow n ((Just x):xs)
    | n `mod` x == 0 = follow (n + 1) xs
    | otherwise = False

split :: Eq a => a -> [a] -> [[a]]
split _ [x] = [[x]]
split s (x:xs)
    | x == s = [] : ys
    | otherwise = (x : head ys) : tail ys
    where ys = split s xs
