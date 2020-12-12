import Control.Monad.State

data Direction = N | E | S | W
    deriving (Show, Eq, Ord, Bounded, Enum)

next :: Direction -> Direction
next W = N
next d = succ d

prev :: Direction -> Direction
prev N = W
prev d = pred d

data Instruction = D Direction Int | L Int | R Int | F Int

data Boat = Boat { facing :: Direction
                 , north :: Int
                 , east :: Int
                 }

data Boat2 = Boat2 { wNorth :: Int
                   , wEast :: Int
                   , bNorth :: Int
                   , bEast :: Int
                   }

parseInst :: String -> Instruction
parseInst (x:xs) = (case x of
                      'N' -> D N
                      'S' -> D S
                      'E' -> D E
                      'W' -> D W
                      'L' -> L
                      'R' -> R
                      'F' -> F) $ read xs

main = do input <- (map parseInst . lines) <$> getContents
          let boat = Boat { facing = E
                          , north = 0
                          , east = 0
                          }
              pos = foldl (flip execInst) boat input
              manhattan = abs (north pos) + abs (east pos)
              boat2 = Boat2 { wNorth = 1
                            , wEast = 10
                            , bNorth = 0
                            , bEast = 0
                            }
              pos2 = foldl (flip execInst2) boat2 input
              manhattan2 = abs (bNorth pos2) + abs (bEast pos2)
          putStrLn . show $ manhattan
          putStrLn . show $ manhattan2

execInst :: Instruction -> Boat -> Boat
execInst (D N n) b = b { north = north b + n }
execInst (D S n) b = b { north = north b - n }
execInst (D E n) b = b { east = east b + n }
execInst (D W n) b = b { east = east b - n }
execInst (F n) b = execInst (D (facing b) n) b
execInst (L n) b = b { facing = iterate prev (facing b) !! div n 90 }
execInst (R n) b = b { facing = iterate next (facing b) !! div n 90 }

execInst2 :: Instruction -> Boat2 -> Boat2
execInst2 (D N n) b = b { wNorth = wNorth b + n }
execInst2 (D S n) b = b { wNorth = wNorth b - n }
execInst2 (D E n) b = b { wEast = wEast b + n }
execInst2 (D W n) b = b { wEast = wEast b - n }
execInst2 (F n) b = b { bNorth = bNorth b + n * wNorth b
                      , bEast = bEast b + n * wEast b
                      }
execInst2 (L n) b = iterate rotate b !! (4 - div n 90)
execInst2 (R n) b = iterate rotate b !! div n 90

rotate :: Boat2 -> Boat2
rotate b = b { wNorth = - wEast b
             , wEast = wNorth b
             }
