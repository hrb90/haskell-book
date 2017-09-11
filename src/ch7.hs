dogYrs :: Integer -> Integer
dogYrs x
  | otherwise = x * 6
  | x <= 0 = 0
  | x <= 1 = x * 15
  | x <= 2 = x * 12
  | x <= 4 = x * 8

avgGrade :: (Fractional a, Ord a) => a -> Char
avgGrade x
  | y >= 0.9 = 'A'
  | y >= 0.8 = 'B'
  | y >= 0.7 = 'C'
  | y >= 0.59 = 'D'
  | otherwise = 'F'
  where y = x / 100

pal :: Eq a => [a] -> Bool
pal xs
  | xs == reverse xs = True
  | otherwise = False

numbers :: (Num a, Ord a) => a -> Integer
numbers x
  | x < 0 = -1
  | x == 0 = 0
  | otherwise = 1

hundredsDigit :: Integral a => a -> a
hundredsDigit x = d
  where xLast = fst $ x `divMod` 100
        d = xLast `mod` 10

foldBoolPattern :: a -> a -> Bool -> a
foldBoolPattern first _ True = first
foldBoolPattern _ second False = second


foldBoolGuard :: a -> a -> Bool -> a
foldBoolGuard first second picker
  | picker = first
  | otherwise = second

g :: (a -> b) -> (a, c) -> (b, c)
g f (a, x) = (f a, x)
