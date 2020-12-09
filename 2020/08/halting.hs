{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE LambdaCase #-}

import Text.Read hiding (step, get)
import Data.Array
import Data.Monoid
import Control.Monad.State

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

data Program = Program { instructions :: Array Int Instruction
                       , executed :: Array Int Bool
                       , accumulator :: Int
                       , position :: Int
                       } deriving (Show)

data Status = Running | Loop | Terminated
    deriving (Show, Eq)

main = do insts <- (map read . lines) <$> getContents
          let len = length insts - 1
              pgm = Program (listArray (0, len) insts) (listArray (0, len) $ repeat False) 0 0
          putStrLn . show . accumulator . execState run $ pgm


exec :: Instruction -> State Program ()
exec (Acc n) = modify $ \p -> p { accumulator = accumulator p + n
                                , position = position p + 1
                                }
exec (Nop _) = modify $ \p -> p { position = position p + 1 }
exec (Jmp n) = modify $ \p -> p { position = position p + n }

step :: State Program Status
step = get >>= \p ->
    if | position p > (snd . bounds . instructions $ p) -> return Terminated
       | executed p ! position p -> return Loop
       | otherwise -> do
           put p { executed = executed p // [(position p, True)] }
           exec $ instructions p ! position p
           return Running

run :: State Program Status
run = step >>= \case Running -> run
                     other -> return other
