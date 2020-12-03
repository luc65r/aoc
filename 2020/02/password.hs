import Data.Bits
import Text.Read
import Data.Char
import Control.Monad

data Pass = Pass { first :: Int
                 , second :: Int
                 , char :: Char
                 , password :: String
                 } deriving (Show)

instance Read Pass where
    readPrec = do
        first <- step readPrec
        getc '-'
        second <- step readPrec
        char <- spaces get
        getc ':'
        password <- spaces getString
        return $ Pass first second char password

getc :: Char -> ReadPrec ()
getc c = (>> pure ()) . guard . (c ==) =<< get

getString :: ReadPrec String
getString = look >>= flip replicateM get . length -- TODO: don't look, just get

spaces :: ReadPrec a -> ReadPrec a
spaces m = look >>= skip
    where skip (c:cs) | isSpace c = get >> skip cs
          skip _ = m


main = do passwords <- (map read . lines) <$> getContents
          print firstPassValid passwords
          print secondPassValid passwords
              where print v = putStrLn . show . length . filter v

firstPassValid, secondPassValid :: Pass -> Bool
firstPassValid p = isBetween . length . filter (== char p) . password $ p
    where isBetween x = first p <= x && x <= second p

secondPassValid p = isCharAt (first p) `xor` isCharAt (second p)
    where isCharAt n = password p !! (n - 1) == char p
