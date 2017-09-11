sumFirstN :: (Num a, Eq a) => a -> a
sumFirstN n
  | n == 0 = 0
  | otherwise = n + sumFirstN (n - 1)

recurMult :: Integral a => a -> a -> a
recurMult m n =
  case n of
    0 -> 0
    _ -> m + recurMult m (n - 1)

mc91 :: (Num a, Ord a) => a -> a
mc91 x
  | x > 100 = x - 10
  | otherwise = mc91 . mc91 $ (x + 11)

digitToWord :: Int -> String
digitToWord n =
  case n of
    0 -> "zero"
    1 -> "one"
    2 -> "two"
    3 -> "three"
    4 -> "four"
    5 -> "five"
    6 -> "six"
    7 -> "seven"
    8 -> "eight"
    9 -> "nine"
    _ -> error "This is not a digit"

digits :: Int -> [Int]
digits n
  | n < 10 = n:[]
  | otherwise = digits firstDigits ++ [lastDigit]
    where (firstDigits, lastDigit) = divMod n 10

intersperse :: a -> [a] -> [a]
intersperse sep list =
  case list of
    [] -> []
    hd:[] -> [hd]
    hd:tl -> hd:sep:(intersperse sep tl)

wordNumber :: Int -> String
wordNumber n =
  concat $ intersperse "-" $ map digitToWord (digits n)
