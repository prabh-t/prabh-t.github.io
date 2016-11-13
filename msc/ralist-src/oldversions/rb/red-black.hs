-- Prabhat
-- Red-black tree representation of list
-- Adapted from Chris Okasaki book
import System.Random
import Data.List

data Color = R | B deriving Show
data RB a = E | T Color (RB a) a (RB a) deriving Show

-- type synonym - parallel list is represented as a tree                                
type ParLst a = RB a

-- the max num of elements in the list of a tree leaf                           
chunksize::Int
chunksize = 2

-- threshold of when a list needs to be represented as a tree
threshold::Int
threshold = 20
                                
chunk :: Ord a => Int -> [a] -> [[a]]
chunk n [] = []
chunk n xs = ys : chunk n zs
  where (ys,zs) = splitAt n xs
  
parLst :: Ord a => [a] -> ParLst [a]
parLst [] = E
parLst xs = lstToTree (chunk chunksize xs)

lstToTree :: Ord a => [[a]] -> ParLst [a]
lstToTree lst
	| length lst == 1 = insertT (head lst) E
	| otherwise = insertT (head lst) (lstToTree (tail lst))
	
sizeT::	RB a -> Int
sizeT (T _ E _ E) = 1
sizeT (T _ l _ E) = 1 + sizeT l
sizeT (T _ E _ r) = 1 + sizeT r
sizeT (T _ l _ r) = 1 + sizeT l + sizeT r

-- insert an item in the tree, rebalancing it where necessary
insertT :: Ord a => a -> RB a -> RB a
insertT x s =
	T B a z b
	where
	T _ a z b = ins s
	ins E = T R E x E
	ins s@(T B a y b) = balance (ins a) y b
	ins s@(T R a y b) = T R (ins a) y b

member :: Ord a => a -> RB a -> Bool
member x E = False
member x (T _ a y b)
	| x<y = member x a
	| x>y = member x b
	| otherwise = True

-- balancing functions
balance :: RB a -> a -> RB a -> RB a
balance (T R a x b) y (T R c z d) = T R (T B a x b) y (T B c z d)
balance (T R (T R a x b) y c) z d = T R (T B a x b) y (T B c z d)
balance (T R a x (T R b y c)) z d = T R (T B a x b) y (T B c z d)
balance a x (T R b y (T R c z d)) = T R (T B a x b) y (T B c z d)
balance a x (T R (T R b y c) z d) = T R (T B a x b) y (T B c z d)
balance a x b = T B a x b

{- deletion a la SMK -}
delete :: Ord a => a -> RB a -> RB a
delete x t =
	case del t of {T _ a y b -> T B a y b; _ -> E}
	where
	del E = E
	del (T _ a y b)
	    | x<y = delformLeft a y b
	    | x>y = delformRight a y b
            | otherwise = app a b
	delformLeft a@(T B _ _ _) y b = balleft (del a) y b
	delformLeft a y b = T R (del a) y b
	delformRight a y b@(T B _ _ _) = balright a y (del b)
	delformRight a y b = T R a y (del b)

balleft :: RB a -> a -> RB a -> RB a
balleft (T R a x b) y c = T R (T B a x b) y c
balleft bl x (T B a y b) = balance bl x (T R a y b)
balleft bl x (T R (T B a y b) z c) = T R (T B bl x a) y (balance b z (sub1 c))

balright :: RB a -> a -> RB a -> RB a
balright a x (T R b y c) = T R a x (T B b y c)
balright (T B a x b) y bl = balance (T R a x b) y bl
balright (T R a x (T B b y c)) z bl = T R (balance (sub1 a) x b) y (T B c z bl)

sub1 :: RB a -> RB a
sub1 (T B a x b) = T R a x b
sub1 _ = error "invariance violation"

app :: RB a -> RB a -> RB a
app E x = x
app x E = x
app (T R a x b) (T R c y d) =
	case app b c of
	    T R b' z c' -> T R(T R a x b') z (T R c' y d)
	    bc -> T R a x (T R bc y d)
app (T B a x b) (T B c y d) = 
	case app b c of
	    T R b' z c' -> T R(T B a x b') z (T B c' y d)
	    bc -> balleft a x (T B bc y d)
app a (T R b x c) = T R (app a b) x c
app (T R a x b) c = T R a x (app b c)

