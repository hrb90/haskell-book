data DayOfWeek =
  Mon | Tue | Weds | Thu | Fri | Sat | Sun
  deriving (Eq, Ord, Show)

data TisAnInteger =
  TisAn Integer

data TwoIntegers =
  Two Integer Integer

data StringOrInt =
  TisAnInt Int
  | TisAString String

data Pair a =
  Pair a a

data Tuple a b =
  Tuple a b

data Which a =
  ThisOne a
  | ThatOne a

data EitherOr a b =
  Hello a
  | Goodbye b

instance Eq TisAnInteger where
  (==) (TisAn x) (TisAn y) = x == y

instance Eq TwoIntegers where
  (==) (Two a0 b0) (Two a1 b1) = (a0 == a1) && (b0 == b1)

instance Eq StringOrInt where
  (==) (TisAnInt x) (TisAnInt y) = (x == y)
  (==) (TisAString x) (TisAString y) = (x == y)
  (==) _ _ = False

instance Eq a => Eq (Pair a) where
  (==) (Pair a0 b0) (Pair a1 b1) = (a0 == a1) && (b0 == b1)

instance (Eq a, Eq b) => Eq (Tuple a b) where
  (==) (Tuple a0 b0) (Tuple a1 b1) = (a0 == a1) && (b0 == b1)

instance Eq a => Eq (Which a) where
  (==) (ThisOne x) (ThisOne y) = (x == y)
  (==) (ThatOne x) (ThatOne y) = (x == y)
  (==) _ _ = False

instance (Eq a, Eq b) => Eq (EitherOr a b) where
  (==) (Hello x) (Hello y) = (x == y)
  (==) (Goodbye x) (Goodbye y) = (x == y)
  (==) _ _ = False

i :: Num a => a
i = 1

f :: RealFrac a => a
f = 5.0

freud :: Int -> Int
freud x = x

myX = 1 :: Int

sigmund :: Int -> Int
sigmund x = myX

chk :: Eq b => (a -> b) -> a -> b -> Bool
chk fn x y = (fn x) == y

arith :: Num b => (a -> b) -> Integer -> a -> b
arith fn power x = (fn x) ^ power
