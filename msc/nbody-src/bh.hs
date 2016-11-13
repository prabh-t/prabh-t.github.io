{-# LANGUAGE CPP, BangPatterns #-}

-- File: bh.hs
-- Description: nbody computation: barnes-hut algoritms
-- Author: Prabhat Totoo

-- This implementation of nbody problem uses the barnes-hut method
-- for the simulation. In this version, we first offsets momentum, 
-- calculates energy before the N steps simulation, performs simulation 
-- and recalculates energy at the end (similar to allpairs.hs).
-- Reads bodies from text file.

-- Compile | Run
-- Seq: > ghc --make -O2 bh.hs | > bh
-- Par: > ghc --make -O2 -threaded -rtsopts bh.hs | > bh +RTS -Nx

-- Runtimes/Output
-- Machine: Intel Xeon CPU E5410 @ 2.33GHz, 8 cores, 8GB RAM (Linux)
-- 16k bodies, 20 iteration
-- #PE	Runtimes	Speedups
-- Seq	7.53		1
-- 1	7.99		0.94
-- 2	5.38		1.40
-- 3	4.35		1.73
-- 4	3.85		1.96
-- 5	3.64		2.07
-- 6	3.41		2.21
-- 7	3.32		2.27
-- 8	3.16		2.38
import IO
import System
import System.Time
import Text.Printf
import Array
import Data.List
#if defined (PAR)
import Control.Parallel
import Control.Parallel.Strategies
import Control.DeepSeq
import GHC.Exts
import GHC.Conc (numCapabilities)
#endif

data P = P {-# UNPACK #-} !Double {-# UNPACK #-} !Double {-# UNPACK #-} !Double
data V = V {-# UNPACK #-} !Double {-# UNPACK #-} !Double {-# UNPACK #-} !Double
data M = M {-# UNPACK #-} !Double
data PM = PM {-# UNPACK #-} !P {-# UNPACK #-} !M

-- acceleration
data Accel = Acc {-# UNPACK #-} !Double {-# UNPACK #-} !Double {-# UNPACK #-} !Double

-- bounding box: a region in 3D space containing points
data Bbox = Bbox {-# UNPACK #-} !P {-# UNPACK #-} !P

-- the BH tree
-- node consists of size, centroid X, Y, Z, mass, and children
data BHTree = BHT {-# UNPACK #-} !Double {-# UNPACK #-} !PM ![BHTree]

#if defined (PAR)
instance NFData (Accel) where
    rnf (Acc x y z) = deepseq (x, y, z) ()
instance NFData (PM) where
    rnf (PM p m) = deepseq (p, m) ()
instance NFData (P) where
    rnf (P x y z) = deepseq (x, y, z) ()
instance NFData (V) where
    rnf (V x y z) = deepseq (x, y, z) ()
instance NFData (M) where
    rnf (M m) = deepseq m ()
instance NFData (BHTree) where
    rnf (BHT s pm ts) = deepseq (s, pm, ts) ()
	
chunksize = (fromInteger n) `quot` (numCapabilities * 1)
#endif

n = 16000
ns = [0..(n-1)]
arrbounds = (0,(n-1))
toArr xs = listArray arrbounds xs

timeStep = 0.001

-- If the distance between the points is smaller than this
eps = 0.01 	-- then ignore the forces between them.

-- If s / d < threshold, then the internal node is sufficiently far away.
threshold = 0.8

genPms tag = PM (P (tag' * 1.0) (tag' * 1.2) (tag' * 0.8)) (M (tag' * 30.0))
   where tag' = fromIntegral tag

genVel tag = V (tag' * 0.1) (tag' * 0.2) (tag' * 0.4)
   where tag' = fromIntegral tag
		
main =
	do
		let 
			pms = toArr $ [ (genPms i) | i <- [0..n-1] ]
			vs = toArr $ [ (genVel i) | i <- [0..n-1] ]
		let res1 = (foldl (\sum i -> sum + g (pms!i) (vs!i)) 0 ns) -- do meaningless sum...
		print (res1)
		t1 <- getClockTime -- ...so that, at this point, the positions, velocities and masses have been read from file
		let (pms',vs') = nSteps 20 pms vs
		let res2 = (foldl (\sum i -> sum + f (pms'!i) (vs'!i)) 0 ns) -- do a meaningless sum to generate a small output
		print (res2)
		t2 <- getClockTime
		putStrLn $ printf "time taken: %.2fs" $ secDiff t1 t2

g (PM (P x y z) (M m)) (V vx vy vz) = x+y+z+m+vx+vy+vz
f (PM (P x y z) _) (V vx vy vz) = x+y+z+vx+vy+vz -- mass does not change, not need to check

nSteps 0 pms vs = (pms,vs)
nSteps n pms vs = nSteps (n-1) pms' vs'
	where
		(pms',vs') = oneStep pms vs

oneStep pms vs = (new_pms,new_vs)
	where
		--new_pms = updatePos pms new_vs
		--new_vs = updateVel vs accel
		--accel = calcAccels pms
		new_pms = toArr $ fst unz
		new_vs = toArr $ snd unz
		unz = unzip new_pms_vs
#if defined (PAR)
		new_pms_vs = map (\i -> dothis tree (pms!i) (vs!i)) ns `using` parListChunk chunksize rdeepseq 
#else
		new_pms_vs = map (\i -> dothis tree (pms!i) (vs!i)) ns
#endif		
		tree = buildTree box (elems pms)
		box = findBounds pms
		
dothis tree pm@(PM (P x y z) m) (V vx vy vz) = (new_pm,new_v)
	where
		Acc x' y' z' = calcAccel tree pm
		new_v = V (vx+x') (vy+y') (vz+z')
		new_pm = PM (P (x + timeStep * vx) (y + timeStep * vy) (z + timeStep * vz)) m		

-- calc the accelerations of all points
#if defined (PAR)
calcAccels pms = toArr ( map (\i -> calcAccel tree (pms!i)) ns `using` parListChunk chunksize rdeepseq )
#else
calcAccels pms = toArr ( map (\i -> calcAccel tree (pms!i)) ns )
#endif
	where
		tree = buildTree box (elems pms)
		box = findBounds pms

-- find the coordinates of the bounding box that contains the given points
findBounds pms = foldl' f (Bbox (P 0 0 0) (P 0 0 0)) ns
	where
		f (Bbox (P minx miny minz) (P maxx maxy maxz)) i =
			let	(PM (P x y z) _) = pms!i
				new_min = P (min minx x) (min miny y) (min minz z)
				new_max = P (max maxx x) (max maxy y) (max maxz z)
			in Bbox new_min new_max

-- build the Barnes-Hut tree
buildTree :: Bbox -> [PM] -> BHTree
buildTree bb pms
 | length pms <= 1	= BHT s pm []
 | otherwise		= BHT s pm subTrees
 where	pm			= calcCentroid pms
	boxesAndPts		= splitPoints bb pms
#if defined (PAR)
	subTrees		= map (\(bb',pms') -> buildTree bb' pms') boxesAndPts `using` parList rdeepseq
#else
	subTrees		= map (\(bb',pms') -> buildTree bb' pms') boxesAndPts
#endif
	(Bbox (P minx miny minz) (P maxx maxy maxz))	= bb
	s			= minimum [abs (maxx - minx), abs (maxy - miny), abs (maxz - minz)]
	
-- if two bodies have positions (x1, y1) and (x2, y2), and masses m1 and m2, 
-- then their total mass and center of mass (x, y) are given by:
--    m = m1 + m2
--    x = (x1*m1 + x2*m2) / m
--    y = (y1*m1 + y2*m2) / m
-- calculate the centroid of points
calcCentroid :: [PM] -> PM
calcCentroid pms = PM  (P (sum xs / mass) (sum ys / mass) (sum zs / mass)) (M mass)
  where
    mass     = foldl' (+) 0 [ m | (PM _ (M m))  <- pms ]
    (xs, ys, zs) = unzip3 [ (m * x, m * y, m * z) | (PM (P x y z) (M m)) <- pms ]   

-- split points according to their locations in the box
splitPoints :: Bbox -> [PM] -> [(Bbox, [PM])]
splitPoints bb []  	= [(bb,[])]
splitPoints bb [pm] = [(bb,[pm])]
splitPoints bb pms  = boxesAndPts
	where	
		Bbox (P minx miny minz) (P maxx maxy maxz) = bb		
		p1		= [pm|pm<-pms,inBox b1 pm]
		p2		= [pm|pm<-pms,inBox b2 pm]
		p3		= [pm|pm<-pms,inBox b3 pm]
		p4		= [pm|pm<-pms,inBox b4 pm]
		p5		= [pm|pm<-pms,inBox b5 pm]
		p6		= [pm|pm<-pms,inBox b6 pm]
		p7		= [pm|pm<-pms,inBox b7 pm]
		p8		= [pm|pm<-pms,inBox b8 pm]
		b1		= Bbox (P minx miny minz) (P midx midy midz)
		b2		= Bbox (P minx midy minz) (P midx maxy midz)
		b3		= Bbox (P midx miny minz) (P maxx midy midz)
		b4		= Bbox (P midx midy minz) (P maxx maxy midz)
		b5		= Bbox (P minx miny midz) (P midx midy maxz)
		b6		= Bbox (P minx midy midz) (P midx maxy maxz)
		b7		= Bbox (P midx miny midz) (P maxx midy maxz)
		b8		= Bbox (P midx midy midz) (P maxx maxy maxz)
		boxes		= b1:b2:b3:b4:b5:b6:b7:b8:[]
		splitPts	= p1:p2:p3:p4:p5:p6:p7:p8:[]
		boxesAndPts = [ (box,pts) | (box,pts) <- zip boxes splitPts, not (null pts) ]
		(midx, midy, midz)	= ((minx + maxx) / 2.0 , (miny + maxy) / 2.0 , (minz + maxz) / 2.0) 


-- check if point is in box
inBox :: Bbox -> PM -> Bool
inBox (Bbox (P x1  y1 z1) (P x2 y2 z2)) (PM (P x y z) _) 
 	= (x > x1) && (x <= x2) && (y > y1) && (y <= y2) && (z > z1) && (z <= z2)


-- calculate the accelleration of a point due to the points in the given tree
calcAccel :: BHTree -> PM -> Accel
calcAccel (BHT s pm subtrees) pm'
	| null subtrees = accel pm pm'
	| isFar s pm pm' = accel pm pm'
	| otherwise	= foldl' addAcc (Acc 0 0 0) [ calcAccel st pm' | st <- subtrees ]
		where
			addAcc (Acc a1 b1 c1) (Acc a2 b2 c2) = Acc (a1+a2) (b1+b2) (c1+c2)

-- calculate the acceleration on a point due to some other point
accel :: PM -> PM -> Accel
accel (PM (P x y z) (M m)) (PM (P x' y' z') (M m')) = Acc (dx * m * mag) (dy * m * mag) (dz * m * mag)
	where
		dsqr = (dx * dx) + (dy * dy) + (dz * dz) + eps
		d    = sqrt dsqr 
		dx   = x' - x
		dy   = y' - y
		dz   = z' - z
		mag = timeStep / (dsqr * d)


-- use centroid as approximation if the point is far from a cell
isFar :: Double -> PM -> PM -> Bool
isFar s (PM (P x y z) _) (PM (P x' y' z') _)
 = let	dx	= x - x'
	dy	= y - y'
	dz	= z - z'
	dist	= sqrt (dx * dx + dy * dy + dz * dz)
   in	(s / dist) < threshold


-- update velocity
#if defined (PAR)
updateVel vs acs = toArr ( map (\i -> updateV (vs!i) (acs!i)) ns `using` parListChunk chunksize rdeepseq )
#else
updateVel vs acs = toArr ( map (\i -> updateV (vs!i) (acs!i)) ns )
#endif
	where
		updateV (V vx vy vz) (Acc x y z) = V (vx+x) (vy+y) (vz+z)

#if defined (PAR)
updatePos pms vs = toArr ( map (\i -> updatePm (pms!i) (vs!i)) ns `using` parListChunk chunksize rdeepseq )
#else
updatePos pms vs = toArr ( map (\i -> updatePm (pms!i) (vs!i)) ns )
#endif
	where
		updatePm (PM (P x y z) m) (V vx vy vz) = PM (P (x + timeStep * vx) (y + timeStep * vy) (z + timeStep * vz)) m

-- func to calc time taken between t0 and t1 in sec
secDiff::ClockTime -> ClockTime -> Float
secDiff (TOD secs1 psecs1) (TOD secs2 psecs2) = fromInteger (psecs2 - psecs1) / 1e12 + fromInteger (secs2 - secs1)
