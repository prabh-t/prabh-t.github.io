-- File   : BinTree_v2.hs
-- Author : Prabhat Totoo
--
-- In this version, instead of storing a single list element
-- on the leaves, atmost 2 elements are stored. The initial
-- list is broken down into sublists of 2 elements each, which 
-- are then placed on the tree leaves.

import Data.List (sort)

-- define tree type which stores element on its leaves
data Tree a = Leaf a | Node (Tree a) (Tree a) deriving (Eq, Ord, Show, Read)

-- use a type synonym for par. list represented as a tree
type ParList a = Tree a

-- num of elements on each tree leaf
numElem::Int
numElem = 2

-- if list contains less than threshold, do not represent as tree
-- NOT USE --
threshold::Int
threshold = 20

-- break a list into a list sublists
chunk :: Int -> [a] -> [[a]]
chunk n [] = []
chunk n xs = ys : chunk n zs
  where (ys,zs) = splitAt n xs

-- convert list to tree
parList :: [a] -> ParList [a]
parList [] = Leaf []
parList xs = listToTree (chunk numElem xs)

listToTree :: [[a]] -> ParList [a]
listToTree lst
  | length lst == 1 = Leaf (head lst)     
  | otherwise = Node (generateTree left) (generateTree right)
  where
    (left, right) = splitAt (length lst `div` 2) lst
    generateTree myLst
      | length myLst == 1 = Leaf (head myLst)
      | otherwise = Node (Leaf (head myLst)) (generateTree (tail myLst))

-- convert from tree to list                        
toList :: ParList [a] -> [a]
toList (Leaf []) = []
toList (Leaf xs) = xs
toList (Node l r) = (toList l ++ rs)
  where rs = toList r

-- naive sort function on tree making use of the Data.List sort function
listSort::Ord a => ParList [a] -> [a]
listSort (Leaf l) = sort l
lstSort (Node (Leaf l) (Leaf r)) = sort l ++ sort r
lstSort (Node l (Leaf r)) = lstSort l ++ sort r
lstSort (Node (Leaf l) r) = sort l ++ lstSort r
lstSort (Node l r) = lstSort l ++ lstSort r

-- another one
treeToListOrd (Leaf xs) = sort xs
treeToListOrd (Node xl xr) = treeToListOrd xl ++ treeToListOrd xr

-- add item to list
add :: a -> ParList [a] -> ParList [a]
add n l = parList (n : (toList l))

-- some test
test = do 
         putStrLn (show t)
         putStrLn (show tt)
         putStrLn (show (toList tt))
         where
           t = parList [1..10]
           tt = add 0 t
		   