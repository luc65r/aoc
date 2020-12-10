import Data.List
import Control.Arrow

main = do input <- (sort . map read . lines) <$> getContents
          let diffs = zipWith (-) input $ 0 : input
              nbEq n = length . filter (== n)
          putStrLn . show . uncurry (*) . (nbEq 1 &&& (+ 1) . nbEq 3) $ diffs
          -- There isn't any 2 gap in diffs
          putStrLn . show . product . map ((tribo !!) . length) . filter ((== 1) . head) . group $ diffs

tribo :: [Int]
tribo = 1 : 1 : 2 : zipWith3 add3 tribo (tail tribo) (tail (tail tribo))
    where add3 x y z = x + y + z
