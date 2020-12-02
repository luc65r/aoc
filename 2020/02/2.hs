import Data.Bits

data Pass = Pass { first :: Int
                 , second :: Int
                 , char :: Char
                 , password :: String
                 } deriving (Show)

main = interact countValidPasswords

countValidPasswords :: String -> String
countValidPasswords = (++ "\n") . show . length . filter (isPassValid . toPass) . lines

isPassValid :: Pass -> Bool
isPassValid p = isCharAt (first p) `xor` isCharAt (second p)
    where isCharAt n = password p !! (n - 1) == char p

toPass :: String -> Pass
toPass s = Pass first second char password
    where first = read . takeWhile (/= '-') $ s
          second = read . takeWhile (/= ' ') . tail . dropWhile (/= '-') $ s
          char = head . tail . dropWhile (/= ' ') $ s
          password = tail . tail . dropWhile (/= ':') $ s
