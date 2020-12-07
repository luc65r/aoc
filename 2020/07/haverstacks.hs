import Control.Arrow
import Data.List

data Bag = Bag String [(Int, Bag)]
    deriving (Show)

main = do input <- getContents
          let bags = foldl toBag [] . map (parseLine . split ' ') . lines $ input
          putStrLn . show . flip (-) 1 . length . filter (contains "shiny gold") $ bags
          putStrLn . show . countBags . findBag "shiny gold" $ bags

countBags :: Bag -> Int
countBags (Bag _ bs) = foldl (\acc -> \(n, bs') -> acc + n + n * countBags bs') 0 $ bs

contains :: String -> Bag -> Bool
contains s (Bag s' bs)
    | s == s' = True
    | otherwise = or . map (contains s . snd) $ bs

toBag :: [Bag] -> (String, [(Int, String)]) -> [Bag]
toBag [] (color, inside) = [Bag color . map (\(n, s) -> (n, Bag s [])) $ inside]
toBag bs (color, inside) = bag : (map (updateBag bag) bs)
    where bag = Bag color (map (second (flip findBag bs)) inside)

findBag :: String -> [Bag] -> Bag
findBag s bs = case find (\(Bag s' _) -> s == s') bs of
                 Just b -> b
                 Nothing -> Bag s []

updateBag :: Bag -> Bag -> Bag
updateBag b1@(Bag c1 _) (Bag c2 l)
    | c1 == c2 = b1
    | otherwise = Bag c2 (map (second (updateBag b1)) l)

parseLine :: [String] -> (String, [(Int, String)])
parseLine (c1:c2:_bags:_contain:ws) = (c1 ++ " " ++ c2, parseContains ws)

parseContains :: [String] -> [(Int, String)]
parseContains [] = []
parseContains (_no:_other:_bags:[]) = []
parseContains (n:c1:c2:_bag:ws) = (read n, c1 ++ " " ++ c2) : parseContains ws

split :: Eq a => a -> [a] -> [[a]]
split _ [x] = [[x]]
split a (x:xs)
    | a == x = []:s
    | otherwise = (x:ys):yss 
    where s@(ys:yss) = split a xs
