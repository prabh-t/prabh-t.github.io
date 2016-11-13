data Set a = Tip | Bin Size a (Set a) (Set a) deriving Show

type Size = Int

toTree :: Ord a => [a] -> Set a
toTree xs
	| length xs == 1 = insert (head xs) Tip
	| otherwise = insert (head xs) (toTree (tail xs))
	
size :: Set a -> Int
size t
  = case t of
      Tip          -> 0
      Bin sz _ _ _ -> sz
	  
delta,ratio :: Int
delta = 4
ratio = 2

insert :: Ord a => a -> Set a -> Set a
insert x t
  = case t of
      Tip -> singleton x
      Bin sz y l r
          -> case compare x y of
               LT -> balance y (insert x l) r
               GT -> balance y l (insert x r)
               EQ -> Bin sz x l r
			   
singleton :: a -> Set a
singleton x 
  = Bin 1 x Tip Tip
  
balance :: a -> Set a -> Set a -> Set a
balance x l r
  | sizeL + sizeR <= 1    = Bin sizeX x l r
  | sizeR >= delta*sizeL  = rotateL x l r
  | sizeL >= delta*sizeR  = rotateR x l r
  | otherwise             = Bin sizeX x l r
  where
    sizeL = size l
    sizeR = size r
    sizeX = sizeL + sizeR + 1
	
-- rotate
rotateL :: a -> Set a -> Set a -> Set a
rotateL x l r@(Bin _ _ ly ry)
  | size ly < ratio*size ry = singleL x l r
  | otherwise               = doubleL x l r
rotateL _ _ Tip = error "rotateL Tip"

rotateR :: a -> Set a -> Set a -> Set a
rotateR x l@(Bin _ _ ly ry) r
  | size ry < ratio*size ly = singleR x l r
  | otherwise               = doubleR x l r
rotateR _ Tip _ = error "rotateL Tip"

-- basic rotations
singleL, singleR :: a -> Set a -> Set a -> Set a
singleL x1 t1 (Bin _ x2 t2 t3)  = bin x2 (bin x1 t1 t2) t3
singleL _  _  Tip               = error "singleL"
singleR x1 (Bin _ x2 t1 t2) t3  = bin x2 t1 (bin x1 t2 t3)
singleR _  Tip              _   = error "singleR"

doubleL, doubleR :: a -> Set a -> Set a -> Set a
doubleL x1 t1 (Bin _ x2 (Bin _ x3 t2 t3) t4) = bin x3 (bin x1 t1 t2) (bin x2 t3 t4)
doubleL _ _ _ = error "doubleL"
doubleR x1 (Bin _ x2 t1 (Bin _ x3 t2 t3)) t4 = bin x3 (bin x2 t1 t2) (bin x1 t3 t4)
doubleR _ _ _ = error "doubleR"	

bin :: a -> Set a -> Set a -> Set a
bin x l r
  = Bin (size l + size r + 1) x l r