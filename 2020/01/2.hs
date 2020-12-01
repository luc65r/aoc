import Data.List

main = interact report

report :: String -> String
report s = case find (\(x, y, z) -> x + y + z == 2020) . triplets . map read . lines $ s of
             Just n -> (++ "\n") . show . (\(x, y, z) -> x * y * z) $ n
             Nothing -> "Couln't find the entries !\n"

pairs :: [a] -> [(a, a)]
pairs [x] = []
pairs (x:xs) = map ((,) x) xs ++ pairs xs

triplets :: [a] -> [(a, a, a)]
triplets [x] = []
triplets (x:xs) = map ((,,) x) (pairs xs) ++ triplets xs
