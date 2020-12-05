import Data.Bits
import Data.List

main = do input <- lines <$> getContents
          let ids = map seatID input
          putStrLn . show . maximum $ ids
          putStrLn . show . missing . sort $ ids

seatID :: String -> Int
seatID s = r * 8 + c
    where r = row . take 7 $ s
          c = column . drop 7 $ s

row, column :: String -> Int
row = bsToInt cToBin
    where cToBin 'F' = 0
          cToBin 'B' = 1
          cToBin _ = error "invalid input"

column = bsToInt cToBin
    where cToBin 'L' = 0
          cToBin 'R' = 1
          cToBin _ = error "invalid input"

bsToInt :: (Char -> Int) -> String -> Int
bsToInt cToBin = foldl (\acc -> (shift acc 1 +) . cToBin) 0

missing :: [Int] -> Int
missing (x:y:xs)
    | y - x == 2 = x + 1
    | otherwise = missing (y:xs)
