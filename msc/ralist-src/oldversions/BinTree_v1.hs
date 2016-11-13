-- File   : BinTree_v1.hs
-- Author : Prabhat Totoo
--
-- Experimenting with converting from list to tree structure
-- and vice-versa

-- define new data type
data Tree a = Empty | Leaf a | Node (Tree a) (Tree a) deriving (Eq, Ord, Show, Read)

-- convert from list to tree
parList :: [a] -> Tree a
parList list | size == 0 = Empty
             | size == 1 = Leaf (head list)
             | otherwise = let (left, right) = splitAt (length list `div` 2) list
			               in Node (generateTree left) (generateTree right)
  where
    size = length list
    generateTree myList | length myList == 1 = Leaf (head myList)
	                    | otherwise = Node (Leaf (head myList)) (generateTree (tail myList))

-- convert tree back to list
toList :: Tree a -> [a]
toList (Leaf n) = [n]
toList (Node l r) = toList l ++ toList r

-- add item to list
-- NOTE: tree is being flattened, item is then prepended and list
-- converted back to tree - not efficient at all
add :: a -> Tree a -> Tree a
add n Empty = Leaf n
add n t = parList (n : (toList t))

-- remove item from list
-- TODO: complete all pattern matching
remove :: a -> Tree a -> Tree a
remove n Empty = error "list empty!"
--remove n (Leaf a) = if n == a then Empty else Empty

-- some test
test = do 
         putStrLn (show t)
         putStrLn (show tt)
         putStrLn (show (toList tt))
         where
           t = parList ['a'..'e']
           tt = add 'i' t