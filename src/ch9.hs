import Data.Char

split :: Eq a => a -> [a] -> [[a]]
split sep list =
  case list of
    [] -> []
    _ -> (takeWhile notSep list):(split sep $ drop 1 $ dropWhile notSep list)
  where notSep = (\x -> x /= sep)

removeArticles :: String -> [String]
removeArticles =
  filter (\x -> not $ elem x articles) . words
  where articles = ["the", "an", "a"]

myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith _ [] _ = []
myZipWith _ _ [] = []
myZipWith f (hd0:tl0) (hd1:tl1) = (f hd0 hd1):(myZipWith f tl0 tl1)

myZip :: [a] -> [b] -> [(a, b)]
myZip = myZipWith (,)

filterUpper :: String -> String
filterUpper = filter isUpper

capitalize :: String -> String
capitalize "" = ""
capitalize (hd:tl) = (toUpper hd):tl

shout :: String -> String
shout = map toUpper

capFirstLetter :: String -> Char
capFirstLetter = toUpper . head

myOr :: [Bool] -> Bool
myOr [] = False
myOr (hd:tl) = if hd then True else myOr tl

myAny :: (a -> Bool) -> [a] -> Bool
myAny f = myOr . map f

myElem :: Eq a => a -> [a] -> Bool
myElem x = any $ (==) x

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (hd:tl) = (myReverse tl) ++ [hd]

squish :: [[a]] -> [a]
squish [] = []
squish (hd:tl) = hd ++ (squish tl)

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap fn = squish . map fn
