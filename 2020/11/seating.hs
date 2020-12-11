import Data.List
import Data.Array
import Control.Monad.State

data Seat = Floor | Empty | Occuped
    deriving (Eq)

instance Show Seat where
    show Floor = "."
    show Empty = "L"
    show Occuped = "#"

type Seats = Array (Int, Int) Seat

cToSeat :: Char -> Seat
cToSeat '.' = Floor
cToSeat 'L' = Empty
cToSeat '#' = Occuped

display :: Seats -> String
display = unlines . map (concatMap $ show . snd) .
    groupBy (\((a, _), _) ((b, _), _) -> a == b) . assocs

main = do input <- (map (map cToSeat) . lines) <$> getContents
          let h = length input
              w = length $ input !! 0
              seats = listArray ((0, 0), (h - 1, w - 1)) $ concat input
              nbOccuped = length . filter (== Occuped) . elems
          putStrLn . show . execState (stabilize $ step updateSeat) $ seats
          putStrLn . show . execState (stabilize $ step updateSeat2) $ seats

neighbours :: [(Int -> Int, Int -> Int)]
neighbours = [ (m , m), (m, id), (m , p)
             , (id, m),          (id, p)
             , (p , m), (p, id), (p , p)
             ]
    where m = flip (-) 1
          p = (+ 1)

updateSeat :: (Int, Int) -> Seats -> Seat
updateSeat p@(y, x) st =
    let neighs = map (st !) . filter (inRange $ bounds st) .
            map (\(fy, fx) -> (fy y, fx x)) $ neighbours
        nbOccuped = length . filter (== Occuped) $ neighs
     in case st ! p of
          Floor -> Floor
          Empty -> if nbOccuped == 0
                      then Occuped
                      else Empty
          Occuped -> if nbOccuped < 4
                        then Occuped
                        else Empty

occDir :: (Int, Int) -> (Int -> Int, Int -> Int) -> Seats -> Bool
occDir p@(y, x) (fy, fx) st
    | not . inRange (bounds st) $ p = False
    | otherwise = case st ! p of
                    Occuped -> True
                    Empty -> False
                    Floor -> occDir (fy y, fx x) (fy, fx) st

updateSeat2 :: (Int, Int) -> Seats -> Seat
updateSeat2 pl@(y, x) st =
    let occupedNeighs = map (\(fy, fx) -> occDir (fy y, fx x) (fy, fx) st) $ neighbours
        nbOccuped = length . filter id $ occupedNeighs
     in case st ! pl of
          Floor -> Floor
          Empty -> if nbOccuped == 0
                      then Occuped
                      else Empty
          Occuped -> if nbOccuped < 5
                        then Occuped
                        else Empty

step :: ((Int, Int) -> Seats -> Seat) -> Seats -> Seats
step f st = listArray (bounds st) . map (flip f st) . indices $ st

stabilize :: (Seats -> Seats) -> State Seats ()
stabilize f = do
    lastState <- get
    modify f
    currentState <- get
    if lastState == currentState
       then return ()
       else stabilize f
