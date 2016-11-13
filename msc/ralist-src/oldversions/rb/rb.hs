data Color = R | B deriving Show
data RedBlackSet a = E | Leaf a | T Color (RedBlackSet a) a (RedBlackSet a) deriving Show

toTree :: Ord a => (a -> RedBlackSet a -> RedBlackSet a) -> [a] -> RedBlackSet a
toTree f xs
	| length xs == 1 = f (head xs) E
	| otherwise = f (head xs) (toTree f (tail xs))

balance B (T R (T R a x b) y c) z d = T R (T B a x b) y (T B c z d)
balance B (T R a x (T R b y c)) z d = T R (T B a x b) y (T B c z d)
balance B a x (T R (T R b y c) z d) = T R (T B a x b) y (T B c z d)
balance B a x (T R b y (T R c z d)) = T R (T B a x b) y (T B c z d)
balance color a x b = T color a x b


insert x s = makeBlack (ins s)
	where
		ins E = T R E x E
		ins (T color a y b)
			| x < y = balance color (ins a) y b
			| x == y = T color a y b
			| x > y = balance color a y (ins b)
		makeBlack (T _ a y b) = T B a y b
		
insertAllowDuplicate x s = makeBlack (ins s)
	where
		ins E = T R E x E
		ins (T color a y b)
			| x <= y = balance color (ins a) y b
			| x > y = balance color a y (ins b)
		makeBlack (T _ a y b) = T B a y b
		
insertInLeaf x s = makeBlack (ins s)
	where
		--ins E = T R E x E
		ins E = ins (Leaf x)
		ins (Leaf x) = T R (Leaf x) 0 E
		ins (T c l v E) = balance c (Leaf x) v l--T R (Leaf x) v l
		ins (T c l v r) = T R (T B (Leaf x) v l) v r
		--ins (T color a y b) = balance color (ins a) y b
		makeBlack (T _ a y b) = T B a y b		

insertOrder x s = makeBlack (ins s)
	where
		ins E = T R E x E
		ins (T color a y b) = balance color (ins a) y b
		makeBlack (T _ a y b) = T B a y b