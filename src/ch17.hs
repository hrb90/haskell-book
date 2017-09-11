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
