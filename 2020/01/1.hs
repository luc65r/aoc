import Data.List

main = interact report

report :: String -> String
report s = case find ((== 2020) . uncurry (+)) . pairs . map read . lines $ s of
             Just n -> (++ "\n") . show . uncurry (*) $ n
             Nothing -> "Couln't find the entries !\n"

pairs :: [a] -> [(a,a)]
pairs [x] = []
pairs (x:xs) = map ((,) x) xs ++ pairs xs
