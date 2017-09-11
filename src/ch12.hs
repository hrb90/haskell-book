data Nat =
  Zero
  | Succ Nat
  deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ x) = 1 + (natToInteger x)

integerToNat :: Integer -> Maybe Nat
integerToNat x
  | x < 0 = Nothing
  | x == 0 = Just Zero
  | x > 0 = (fmap Succ) $ integerToNat (x - 1)

isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust Nothing = False

isNothing :: Maybe a -> Bool
isNothing = not . isJust

mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee defVal _ Nothing = defVal
mayybee _ fn (Just x) = fn x

fromMaybe :: a -> Maybe a -> a
fromMaybe _ (Just x) = x
fromMaybe defVal Nothing = defVal
