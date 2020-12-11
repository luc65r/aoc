import qualified Data.Map as M

data Seat = Floor | Empty | Occuped
    deriving (Show, Eq)

type Seats = M.Map (Int, Int) Seat

cToSeat :: Char -> Seat
cToSeat '.' = Floor
cToSeat 'L' = Empty
cToSeat '#' = Occuped

main = do input <- lines <$> getContents
          let seats = M.fromList . makeKV . map (map cToSeat) $ input
          putStrLn . show . M.size . M.filter (== Occuped) . stabilize (oneRound updateSeat) $ seats
          --putStrLn . show . stabilize (oneRound updateSeat2) $ seats
          putStrLn . show . M.size . M.filter (== Occuped) . stabilize (oneRound updateSeat2) $ seats

mtl :: [Maybe a] -> [a]
mtl [] = []
mtl (Nothing:xs) = mtl xs
mtl ((Just x):xs) = x : (mtl xs)

makeKV :: [[Seat]] -> [((Int, Int), Seat)]
makeKV = foldl (\acc (y, ss) -> acc ++
    map (\(x, s) -> ((y, x), s)) (zip [0..] ss)) [] . zip [0..]


oneRound :: ((Int, Int) -> Seats -> Seat) -> Seats -> Seats
oneRound f st = M.mapWithKey (\p _ -> f p st) st

updateSeat :: (Int, Int) -> Seats -> Seat
updateSeat p@(y, x) st = let neighPlace = [ (y-1, x-1), (y-1, x), (y-1, x+1)
                                          , (y  , x-1),           (y  , x+1)
                                          , (y+1, x-1), (y+1, x), (y+1, x+1)
                                          ]
                             neigh = mtl . map (flip M.lookup st) $ neighPlace
                             nbOccuped = length . filter (== Occuped) $ neigh
                          in case st M.! p of
                               Floor -> Floor
                               Empty -> if nbOccuped == 0
                                           then Occuped
                                           else Empty
                               Occuped -> if nbOccuped < 4
                                             then Occuped
                                             else Empty

occDir :: (Int, Int) -> (Int -> Int, Int -> Int) -> Seats -> Bool
occDir p@(y, x) (fy, fx) st = case M.lookup p st of
                                Nothing -> False
                                Just s -> case s of
                                            Occuped -> True
                                            Empty -> False
                                            Floor -> occDir (fy y, fx x) (fy, fx) st

updateSeat2 :: (Int, Int) -> Seats -> Seat
updateSeat2 pl@(y, x) st = let m = flip (-) 1
                               p = (+ 1)
                               dirs = [ (m , m), (m, id), (m , p)
                                      , (id, m),          (id, p)
                                      , (p , m), (p, id), (p , p)
                                      ]
                               nbOccuped = length . filter id . map (\(fy, fx) -> occDir (fy y, fx x) (fy, fx) st) $ dirs
                           in case st M.! pl of
                                Floor -> Floor
                                Empty -> if nbOccuped == 0
                                            then Occuped
                                            else Empty
                                Occuped -> if nbOccuped < 5
                                              then Occuped
                                              else Empty

stabilize :: (Seats -> Seats) -> Seats -> Seats
stabilize f st = let updated = f st
                  in if st == updated
                        then updated
                        else stabilize f updated
