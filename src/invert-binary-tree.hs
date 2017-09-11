{-# LANGUAGE DeriveFunctor #-}

data MaybePair a = Pair (Maybe a) (Maybe a)
  deriving (Eq, Show, Functor)

data BinaryTree' a =
  Node a (MaybePair (BinaryTree' a))
  deriving (Eq, Show)

invert' :: BinaryTree' a -> BinaryTree' a
invert' (Node x children) = Node x $ (fmap invert') $ invertPair children
  where invertPair (Pair left right) = Pair right left
