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

validate :: Integer -> Bool
validate x = f $ (sumDigits . doubleEveryOther . toDigits) x `mod` 10
    where f y = if y == 0 then True else False

type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi i p1 p2 p3 = [(p1, p2), (p2, p3)]

