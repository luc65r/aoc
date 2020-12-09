import Data.Monoid
import Control.Monad.State

main = do input <- (map read . lines) <$> getContents
          let ns = firstNonSum $ input
              l = evalState (findEq ns input) (0, (0, 0), False)
          putStrLn . show $ ns
          putStrLn . show $ maximum l + minimum l

firstNonSum :: [Int] -> Int
firstNonSum xs
    | isSum x (take 25 xs) = firstNonSum $ tail xs
    | otherwise = x
    where x = xs !! 25

findEq :: Int -> [Int] -> State (Int, (Int, Int), Bool) [Int]
findEq n xs = do
    (s, (a, b), l) <- get
    case compare s n of
      EQ -> return . take (b - a) . drop a $ xs
      LT -> do
          put (s + xs !! b, (a, b + 1), False)
          findEq n xs
      GT -> do
          if l
             then put (s - xs !! (b - 1), (a, b - 1), True)
             else put (s - xs !! a, (a + 1, b), True)
          findEq n xs

isSum :: Int -> [Int] -> Bool
isSum n = any ((== n) . uncurry (+)) . pairs

pairs :: [a] -> [(a, a)]
pairs [x] = []
pairs (x:xs) = map ((,) x) xs ++ pairs xs
