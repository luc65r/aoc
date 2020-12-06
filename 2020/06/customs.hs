import Data.List hiding (group)

main = do input <- group <$> getContents
          putStrLn . show . sum . map (length . nub . filter (/= '\n')) $ input
          putStrLn . show . sum . map (length . foldl1 intersect . lines) $ input

group :: String -> [String]
group "" = []
group (c:c':cs)
    | [c, c'] == "\n\n" = "":(group cs)
    | otherwise = let (y:ys) = group (c':cs)
                   in (c:y):ys
group s = [s]
