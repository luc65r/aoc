import Data.List
import Data.Maybe
import Text.Read

main = do
    ts <- read <$> getLine
    ids <- (map readMaybe . split ',') <$> getLine
    putStrLn . show . uncurry (*)
      . minimumBy (\(_, x) (_, y) -> compare x y)
      . map (\(Just x) -> (x, x - (ts `mod` x)))
      . filter isJust $ ids

    let ns :: [(Integer, Integer)]
        ns = map (\(a, Just n) -> (a `mod` n, n)) . filter (isJust . snd) . zip [0..] $ ids
        p = foldl ((. snd) . (*)) 1 ns
        aes = do
            (a, n) <- ns
            let inv = p `div` n
                (_, _, v) = euclid n inv
                e = v * inv
            return $ a * e
    putStrLn . show . (`mod` p) . negate . sum $ aes

-- https://fr.wikipedia.org/wiki/Algorithme_d%27Euclide_%C3%A9tendu#Pseudo-code
euclid :: Integral a => a -> a -> (a, a, a)
euclid a b = eucl a 1 0 b 0 1
eucl r u v 0 u' v' = (r, u, v)
eucl r u v r' u' v' = eucl r' u' v' (r - r'' * r') (u - r'' * u') (v - r'' * v')
    where r'' = r `div` r'

split :: Eq a => a -> [a] -> [[a]]
split _ [x] = [[x]]
split s (x:xs)
    | x == s = [] : ys
    | otherwise = (x : head ys) : tail ys
    where ys = split s xs
