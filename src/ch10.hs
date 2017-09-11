myLength :: [a] -> Int
myLength =
  foldr add1 0
  where add1 _ n = n + 1

myOr :: [Bool] -> Bool
myOr =
  foldr (||) False

myAny :: (a -> Bool) -> [a] -> Bool
myAny pred =
  myOr . map pred

myElem :: Eq a => a -> [a] -> Bool
myElem x =
  foldr (\a b -> b || (x == a)) False

myReverse :: [a] -> [a]
myReverse =
  foldl (flip (:)) []

myMap :: (a -> b) -> [a] -> [b]
myMap fn =
  foldr ((:) . fn) []

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter pred =
  foldr chooser []
  where chooser x = if pred x then ((:) x) else id

squish :: [[a]] -> [a]
squish = foldr (++) []

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap fn =
  foldr (\x -> (++) (fn x)) []

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id
