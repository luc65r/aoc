{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE LambdaCase #-}

import Text.Read hiding (step, get)
import Data.Array
import Data.Monoid
import Control.Monad.RWS
import Control.Arrow

data Instruction = Acc Int | Jmp Int | Nop Int
    deriving (Show)

instance Read Instruction where
    readPrec = do
        Ident op <- lexP
        Symbol sign <- lexP
        n <- readPrec
        let arg = case sign of
                    "+" -> n
                    "-" -> -n
        return $ case op of
                   "acc" -> Acc arg
                   "jmp" -> Jmp arg
                   "nop" -> Nop arg

flipInstruction :: Instruction -> Instruction
flipInstruction (Acc n) = Acc n
flipInstruction (Nop n) = Jmp n
flipInstruction (Jmp n) = Nop n

data Status = Running | Loop | Terminated
    deriving (Show, Eq)

main = do insts <- (map read . lines) <$> getContents
          let len = length insts - 1
          putStrLn . show . getSum . snd . execRWS run (listArray (0, len) insts) $ (0, listArray (0, len) $ repeat False)


exec :: Instruction -> RWS a (Sum Int) (Int, b) ()
exec (Acc n) = tell (Sum n) >> modify (first (+ 1))
exec (Nop _) = modify $ first (+ 1)
exec (Jmp n) = modify $ first (+ n)

step :: RWS (Array Int Instruction) (Sum Int) (Int, Array Int Bool) Status
step = do
    (p, e) <- get
    insts <- ask
    if | p > (snd . bounds $ insts) -> return Terminated
       | e ! p -> return Loop
       | otherwise -> do
           put (p, e // [(p, True)])
           exec $ insts ! p
           return Running

run :: RWS (Array Int Instruction) (Sum Int) (Int, Array Int Bool) Status
run = step >>= \case Running -> run
                     other -> return other
