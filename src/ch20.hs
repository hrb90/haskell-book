import Data.Monoid

sum' :: (Foldable t, Num a) => t a -> a
sum' = getSum . foldMap Sum

product' :: (Foldable t, Num a) => t a -> a
product' = getProduct . foldMap Product

elem' :: (Foldable t, Eq a) => a -> t a -> Bool
elem' x = getAny . foldMap (Any . ((==) x))

minimum' :: (Foldable t, Ord a) => t a -> a
minimum' = undefined

maximum' :: (Foldable t, Ord a) => t a -> a
maximum' = undefined

null' :: Foldable t => t a -> Bool
null' = getAny . foldMap (const (Any True))

length' :: Foldable t => t a -> Integer
length' = getSum . foldMap (const (Sum 1 :: Sum Integer))

toList' :: Foldable t => t a -> [a]
toList' = foldr (:) []

fold' :: (Foldable t, Monoid m) => t m -> m
fold' = foldMap id

foldMap' :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap' fn = foldr (mappend . fn) mempty

filter' :: (a -> Bool) -> [a] -> [a]
filter' pred = foldr (:) []

filterF :: ( Applicative f
           , Foldable t
           , Monoid (f a)) => (a -> Bool) -> t a -> f a
filterF pred = foldMap (filterFHelper pred)
    where filterFHelper pred x
                | pred x = pure x
                | otherwise = mempty
