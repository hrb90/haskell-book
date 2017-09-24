import Control.Applicative
import Data.Monoid ((<>))
import Data.List (elemIndex)

added :: Maybe Integer
added = (+3) <$> (lookup 3 $ zip [1, 2, 3] [4, 5, 6])

x :: Maybe Int
x = elemIndex 3 [1, 2, 3, 4, 5]

y :: Maybe Int
y = elemIndex 4 [1, 2, 3, 4, 5]

max' :: Int -> Int -> Int
max' = max

maxed :: Maybe Int
maxed = max' <$> x <*> y

newtype Identity a = Identity a
  deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap fn (Identity x) = Identity (fn x)

instance Applicative Identity where
  pure = Identity
  (<*>) (Identity fn) (Identity x) = Identity (fn x)

newtype Constant a b =
  Constant { getConstant :: a }
  deriving (Eq, Ord, Show)

instance Functor (Constant a) where
  fmap _ (Constant { getConstant = x}) = Constant { getConstant = x }

instance Monoid a => Applicative (Constant a) where
  pure x = Constant { getConstant = mempty }
  (<*>) (Constant { getConstant = x1 }) (Constant { getConstant = x2 }) = Constant { getConstant = x1 <> x2 }


e1 :: Maybe String  
e1 = const <$> Just "Hello" <*> pure "World"

data List a =
  Nil
  | Cons a (List a)
  deriving (Eq, Show)

append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys = Cons x $ xs `append` ys

fold :: (a -> b -> b) -> b -> List a -> b
fold _ b Nil = b
fold f b (Cons h t) = f h (fold f b t)

concat' :: List (List a) -> List a
concat' = fold append Nil

instance Functor List where
  fmap fn Nil = Nil
  fmap fn (Cons hd tl) = Cons (fn hd) (fmap fn tl)

instance Applicative List where
  pure x = Cons x Nil
  (<*>) _ Nil = Nil
  (<*>) Nil _ = Nil
  (<*>) (Cons f rest) l = (f <$> l) `append` (rest <*> l)

take' :: Int -> List a -> List a
take' 0 _ = Nil
take' _ Nil = Nil
take' n (Cons hd tl) = Cons hd $ take' (n - 1) tl

newtype ZipList' a =
  ZipList' (List a)
  deriving (Eq, Show)

-- instance Eq a => EqProp (ZipList' a) where
--   xs =-= ys = xs' `eq` ys'
--     where xs' = let (ZipList' l) = xs in take' 3000 l
--           ys' = let (ZipList' l) = ys in take' 3000 l

zipCons :: a -> ZipList' a -> ZipList' a
zipCons x (ZipList' l) = ZipList' $ Cons x l

instance Functor ZipList' where
  fmap f (ZipList' xs) = ZipList' $ fmap f xs

instance Applicative ZipList' where
  pure x = ZipList' $ Cons x Nil
  (<*>) (ZipList' Nil) _ = ZipList' Nil
  (<*>) _ (ZipList' Nil) = ZipList' Nil
  (<*>) (ZipList' (Cons fnHd fnTl)) (ZipList' (Cons valHd valTl)) = zipCons (fnHd valHd) (apRest fnTl valTl)
    where apRest f v = (ZipList' f) <*> (ZipList' v)

data Validation e a =
  Failure e
  | Success a
  deriving (Eq, Show)

  -- same as Either
instance Functor (Validation e) where
  fmap fn (Success x) = Success $ fn x
  fmap fn (Failure y) = Failure y

-- This is different
instance Monoid e => Applicative (Validation e) where
  pure = Success
  (<*>) (Failure e1) (Failure e2) = Failure $ e1 <> e2
  (<*>) (Failure e) (Success _) = Failure e
  (<*>) (Success _) (Failure e) = Failure e
  (<*>) (Success f) (Success x) = Success $ f x