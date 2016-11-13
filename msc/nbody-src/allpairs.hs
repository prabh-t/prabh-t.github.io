{-# LANGUAGE CPP, BangPatterns #-}

-- File: allpairs.hs
-- Description: nbody computation: full all-pairs version
-- Author: Prabhat Totoo

-- The algorithm first offsets momentum, calculates energy before
-- the N steps simulation, performs simulation and recalculates
-- energy at the end.
-- Reads bodies from text file.

-- Compile | Run
-- Seq: > ghc --make -O2 allpairs.hs | > allpairs
-- Par: > ghc --make -O2 -threaded -rtsopts allpairs.hs | > allpairs +RTS -Nx

-- Runtimes/Output (1024 bodies, 20 iterations)
-- Machine: Intel Xeon CPU E5506 @ 2.13GHz, 8 cores, 12GB RAM (Linux)
--		Runtimes	Speedup
-- Seq	4.60		1
-- 1PE	4.63		0.99
-- 2PEs	3.32		1.39
-- 4PEs	2.37		1.94
-- 8PEs	1.56		2.95

import IO
import System
import System.Time
import Array
import Data.List
import Control.Parallel
import Control.Parallel.Strategies
import Control.DeepSeq
import GHC.Exts
import GHC.Conc (numCapabilities)

data M = M {-# UNPACK #-} !Double
data P = P {-# UNPACK #-} !Double {-# UNPACK #-} !Double {-# UNPACK #-} !Double
data V = V {-# UNPACK #-} !Double {-# UNPACK #-} !Double {-# UNPACK #-} !Double

instance NFData (M) where
    rnf (M m) = deepseq m ()	
instance NFData (P) where
    rnf (P x y z) = deepseq (x, y, z) ()	
instance NFData (V) where
    rnf (V vx vy vz) = deepseq (vx, vy, vz) ()	
chunksize = (fromInteger n) `quot` (numCapabilities * 2)

n = 1024
ns = [0..(n-1)]
arrbounds = (0,(n-1))
toArr xs = listArray arrbounds xs

timeStep = 0.001

-- epsilon
eps = 0.01

toDouble s = read s :: Double
word ws i = toDouble (ws!!(fromInteger i))

next s = [a..b] where a = (n*s)+1; b = (n*s)+n

main =
	do
		str <- readFile "../bodies/bodies.txt" -- positions, velocities and masses for 1024 bodies in file
		let 
			ws 	= words str
			nb 	= ws!!0 -- num of bodies
			ms 	= toArr [M (word ws i)|i<-(next 0)]
			x 	= [word ws i|i<-(next 1)]
			y 	= [word ws i|i<-(next 2)]
			z 	= [word ws i|i<-(next 3)]
			vx 	= [word ws i|i<-(next 4)]
			vy 	= [word ws i|i<-(next 5)]
			vz 	= [word ws i|i<-(next 6)]
			pos = toArr $ zipWith3 (\a b c -> P a b c) x y z
			vel = toArr $ zipWith3 (\a b c -> V a b c) vx vy vz
		let vs' = offsetMomentum vel ms
		print (energy pos ms vs')
		let (ps',vs'') = nSteps 20 (pos,vs',ms)
		print (energy ps' ms vs'')

nSteps 0 (ps,vs,ms) = (ps,vs)
nSteps n (ps,vs,ms) = nSteps (n-1) step
	where
		step = oneStep (ps,vs,ms)	

oneStep (ps,vs,ms) = (new_ps,new_vs,ms)
	where
		new_ps = updatePos ps new_vs		
		new_vs = updateVel ps vs ms


updateVel ps vs ms = toArr ( map (\i -> f (vs!i) i) ns `using` parListChunk chunksize rdeepseq )
	where
		f v i = foldl' (deductChange) v (map (\j -> g i j) ns)
		g i j
			| i == j = V 0 0 0
			| otherwise = V (dx * mass_j * mag) (dy * mass_j * mag) (dz * mass_j * mag)
				where
					mag = timeStep / (dSquared * distance)
					distance = sqrt (dSquared)
					dSquared = dx*dx + dy*dy + dz*dz + eps
					dx = ix - jx
					dy = iy - jy
					dz = iz - jz
					P ix iy iz = ps!i
					P jx jy jz = ps!j
					M mass_j = ms!j

updatePos ps vs = toArr ( map (\i -> updateP (ps!i) (vs!i)) ns `using` parListChunk chunksize rdeepseq )
	where
		updateP (P x y z) (V vx vy vz) = P (x + timeStep * vx) (y + timeStep * vy) (z + timeStep * vz)

		
-- calculate the energy   
energy ps ms vs = foldl' addE 0 ns
	where
		addE acc i = (acc + e) - (foldl' minusE 0 [(i+1)..(n-1)])
			where
				e = 0.5 * (mass_i) * (vx*vx + vy*vy + vz*vz)
				P ix iy iz = ps!i
				M mass_i = ms!i
				V vx vy vz = vs!i
				minusE acc' j = (acc' + e')
					where
						e' = (mass_i * mass_j) / distance
						distance = sqrt dSquared
						dSquared = dx*dx + dy*dy + dz*dz + eps
						dx = ix - jx
						dy = iy - jy
						dz = iz - jz
						P jx jy jz = ps!j
						M mass_j = ms!j	
						
offsetMomentum vs ms = toArr new_vs
	where
		new_vs = v1:(tail (elems vs))
		v1 = V (negate (px/m1)) (negate (py/m1)) (negate (pz/m1))
		M m1 = ms!0
		(px,py,pz) = (foldl' f (0,0,0) ns)
		f (x,y,z) i = (vx*m+x, vy*m+y, vz*m+z)
			where
				V vx vy vz = vs!i
				M m = ms!i

deductChange (V vx1 vy1 vz1) (V vx2 vy2 vz2) = V (vx1-vx2) (vy1-vy2) (vz1-vz2)

-- func to calc time taken between t0 and t1 in sec
secDiff::ClockTime -> ClockTime -> Float
secDiff (TOD secs1 psecs1) (TOD secs2 psecs2) = fromInteger (psecs2 - psecs1) / 1e12 + fromInteger (secs2 - secs1)
