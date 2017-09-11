data Price = Price Integer deriving (Eq, Show)

data Size = Size Integer deriving (Eq, Show)

data Manufacturer = Mini | Mazda | Tata deriving (Eq, Show)

data Airline = PapuAir | CatapultsR'Us | TakeYourChancesUnited deriving (Eq, Show)

data Vehicle = Car Manufacturer Price | Plane Airline Size deriving (Eq, Show)

myCar = Car Mini (Price 14000)
urCar = Car Mazda (Price 20000)
clownCar = Car Tata (Price 7000)
doge = Plane PapuAir (Size 1000)

isCar :: Vehicle -> Bool
isCar (Car _ _) = True
isCar _ = False

isPlane :: Vehicle -> Bool
isPlane (Plane _ _) = True
isPlane _ = False

areCars = map isCar

getManu :: Vehicle -> Maybe Manufacturer
getManu (Car m _) = Just m
getManu _ = Nothing

data BinaryTree a =
  Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)

instance Functor BinaryTree where
  fmap _ Leaf = Leaf
  fmap fn (Node left nodeData right) = Node (fmap fn left) (fn nodeData) (fmap fn right)

insert :: Ord a => a -> BinaryTree a -> BinaryTree a
insert x Leaf = Node Leaf x Leaf
insert x (Node lt nodeData rt)
  | x == nodeData = Node lt nodeData rt
  | x < nodeData = Node (insert x lt) nodeData rt
  | x > nodeData = Node lt nodeData (insert x rt)

insertMany :: Ord a => [a] -> BinaryTree a -> BinaryTree a
insertMany [] = id
insertMany (hd:tl) = insertMany tl . insert hd


preorder :: BinaryTree a -> [a]
preorder Leaf = []
preorder (Node lt nodeData rt) = nodeData : (preorder lt) ++ (preorder rt)

inorder :: BinaryTree a -> [a]
inorder Leaf = []
inorder (Node lt nodeData rt) = (inorder lt) ++ (nodeData : (inorder rt))

postorder :: BinaryTree a -> [a]
postorder Leaf = []
postorder (Node lt nodeData rt) = (postorder lt) ++ (postorder rt) ++ [nodeData]
