import Data.List

main = do
    input <- (reverse . map read . splitComma) <$> getLine
    putStrLn . show . head . (!! (2020 - length input)) . iterate (\xs -> next xs : xs) $ input

next :: [Int] -> Int
next (x:xs) = case findIndex (== x) xs of
            Nothing -> 0
            Just n -> n + 1

splitComma :: String -> [String]
splitComma [c] = [[c]]
splitComma (c:cs)
    | c == ',' = []:s
    | otherwise = (c:ys):yss
    where s@(ys:yss) = splitComma cs
