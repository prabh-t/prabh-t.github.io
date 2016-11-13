{-# LANGUAGE CPP, BangPatterns #-}

-- File: allpairs-final.hs
-- Description: nbody computation: full all-pairs version
-- Author: Prabhat Totoo

-- The algorithm is different from allpairs.hs as it does not 
-- offset momentum and calculate energy before and after simulation.
-- It is intended to measure the time for N steps of the simulation.
-- Bodies are generated in the code but this is not included in the time.

-- Compile | Run
-- Seq: > ghc --make -O2 allpairs-final.hs | > allpairs-final
-- Par: > ghc --make -O2 -threaded -rtsopts allpairs-final.hs | > allpairs-final +RTS -Nx

-- Runtimes/Output
-- Machine: Intel Xeon CPU E5410 @ 2.33GHz, 8 cores, 8GB RAM (Linux)
-- 16k bodies, 1 iteration
-- #PE	Runtimes	Speedups
-- Seq	47.71		1
-- 1	50.75		0.94
-- 2	26.06		1.83
-- 3	17.07		2.79
-- 4	12.30		3.88
-- 5	9.70		4.92
-- 6	8.70		5.48
-- 7	7.70		6.20
-- 8	6.89		6.92
import IO
import System
import System.Time
import Text.Printf
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

n = 16000
ns = [0..(n-1)]
arrbounds = (0,(n-1))
toArr xs = listArray arrbounds xs

timeStep = 0.001

-- epsilon
eps = 0.01

genPos tag = P (tag' * 1.0) (tag' * 1.2) (tag' * 0.8)
   where tag' = fromIntegral tag

genVel tag = V (tag' * 0.1) (tag' * 0.2) (tag' * 0.4)
   where tag' = fromIntegral tag

genMass tag = M (tag' * 30.0)
   where tag' = fromIntegral tag

main =
	do
		let 
			ms = toArr $ [ (genMass i) | i <- [0..n-1] ]
			pos = toArr $ [ (genPos i) | i <- [0..n-1] ]
			vel = toArr $ [ (genVel i) | i <- [0..n-1] ]
		let res1 = (foldl (\sum i -> sum + g (pos!i) (vel!i) (ms!i)) 0 ns) -- do meaningless sum...
		print (res1)
		t1 <- getClockTime -- ...so that, at this point, the positions, velocities and masses have been read from file
		let (pos',vel') = nSteps 1 (pos,vel,ms)
		let res2 = (foldl (\sum i -> sum + f (pos'!i) (vel'!i)) 0 ns) -- do a meaningless sum to generate a small output
		print (res2)
		t2 <- getClockTime
		putStrLn $ printf "time taken: %.2fs" $ secDiff t1 t2

g (P x y z) (V vx vy vz) (M m) = x+y+z+vx+vy+vz+m
f (P x y z) (V vx vy vz) = x+y+z+vx+vy+vz

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

deductChange (V vx1 vy1 vz1) (V vx2 vy2 vz2) = V (vx1-vx2) (vy1-vy2) (vz1-vz2)

-- func to calc time taken between t0 and t1 in sec
secDiff::ClockTime -> ClockTime -> Float
secDiff (TOD secs1 psecs1) (TOD secs2 psecs2) = fromInteger (psecs2 - psecs1) / 1e12 + fromInteger (secs2 - secs1)
