import Data.List

main = do input <- (map read . lines) <$> getContents
          putStrLn . toStr . report2 $ input
          putStrLn . toStr . report3 $ input
              where toStr (Just n) = show n
                    toStr Nothing = "Couln't find the entries!"

report2, report3 :: [Int] -> Maybe Int
report2 = fmap (uncurry (*)) . find ((== 2020) . uncurry (+)) . pairs
report3 = fmap (fold3 (*)) . find ((== 2020) . fold3 (+)) . triplets

pairs :: [a] -> [(a, a)]
pairs [x] = []
pairs (x:xs) = map ((,) x) xs ++ pairs xs

triplets :: [a] -> [(a, a, a)]
triplets [x] = []
triplets (x:xs) = map (\(y, z) -> (x, y, z)) (pairs xs) ++ triplets xs

fold3 :: (a -> b -> a) -> (a, b, b) -> a
fold3 f (a, b, c) = f (f a b) c
