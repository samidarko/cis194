import qualified Data.Char as Char

toDigits :: Integer -> [Integer]
toDigits i
    | i <= 0 = []
    | otherwise = f $ show i
    where f (x:[]) = (toInteger $ Char.digitToInt x) : []
          f (x:xs) = (toInteger $ Char.digitToInt x) : f xs

toDigitsRev :: Integer -> [Integer]
toDigitsRev i = reverse $ toDigits i

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther x = reverse [ if snd x == True then fst x * 2 else fst x | x <- zip (reverse x) (take (length x) $ cycle [False, True])]

sumDigits :: [Integer] -> Integer
sumDigits x = sum $ map (sum . toDigits) x
