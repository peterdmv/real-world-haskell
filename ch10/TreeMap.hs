module TreeMap where

data Tree a = Node (Tree a) (Tree a)
            | Leaf a
              deriving (Show)

treeLength (Leaf s) = Leaf (length s)
treeLength (Node l r) = Node (treeLength l) (treeLength r)

treeMap :: (a -> b) -> Tree a -> Tree b
treeMap f (Leaf a) = Leaf (f a)
treeMap f (Node l r) = Node (treeMap f l) (treeMap f r)


instance Functor Tree where
  fmap = treeMap

-- instance Functor [] where
--   fmap = map

