data Pass = Pass { pmin :: Int
                 , pmax :: Int
                 , char :: Char
                 , password :: String
                 } deriving (Show)

main = interact countValidPasswords

countValidPasswords :: String -> String
countValidPasswords = (++ "\n") . show . length . filter (isPassValid . toPass) . lines

isPassValid :: Pass -> Bool
isPassValid p = (\x -> x >= pmin p && x <= pmax p) . length . filter (== char p) . password $ p

toPass :: String -> Pass
toPass s = Pass pmin pmax char password
    where pmin = read . takeWhile (/= '-') $ s
          pmax = read . takeWhile (/= ' ') . tail . dropWhile (/= '-') $ s
          char = head . tail . dropWhile (/= ' ') $ s
          password = tail . tail . dropWhile (/= ':') $ s
