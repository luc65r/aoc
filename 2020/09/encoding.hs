import Data.Monoid
import Data.List
import Control.Monad.State

main = do input <- (map read . lines) <$> getContents
          let ns = firstNonSum $ input
              (a, n) = evalState (findEq ns input) []
              l = take n . drop a $ input
          putStrLn . show $ ns
          putStrLn . show $ maximum l + minimum l

firstNonSum :: [Int] -> Int
firstNonSum xs
    | isSum x (take 25 xs) = firstNonSum $ tail xs
    | otherwise = x
    where x = xs !! 25

findEq :: Int -> [Int] -> State [Maybe Int] (Int, Int)
findEq n (x:xs) = do
    modify $ map (mfilter (<= n) . fmap (+ x)) . (return 0 :)
    m <- get
    case findIndex (any (== n)) m of
        Just i -> let a = length m - i - 1
                   in return (a, i)
        Nothing -> findEq n xs

isSum :: Int -> [Int] -> Bool
isSum n = any ((== n) . uncurry (+)) . pairs

pairs :: [a] -> [(a, a)]
pairs [x] = []
pairs (x:xs) = map ((,) x) xs ++ pairs xs
