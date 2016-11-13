{-
 - Intel Concurrent Collections for Haskell
 - Copyright (c) 2010, Intel Corporation.
 -
 - This program is free software; you can redistribute it and/or modify it
 - under the terms and conditions of the GNU Lesser General Public License,
 - version 2.1, as published by the Free Software Foundation.
 -
 - This program is distributed in the hope it will be useful, but WITHOUT
 - ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 - FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public License for
 - more details.
 -
 - You should have received a copy of the GNU Lesser General Public License along with
 - this program; if not, write to the Free Software Foundation, Inc., 
 - 51 Franklin St - Fifth Floor, Boston, MA 02110-1301 USA.
 -
 -}
{-# LANGUAGE CPP, MagicHash, UnboxedTuples, BangPatterns #-}

-- Author: Chih-Ping Chen

-- Prabhat Totoo
-- Modified to perform n steps (iterations)

-- This program uses CnC to calculate the accelerations of the bodies in a 3D system.  


-- Runtimes/Output
-- Machine: Intel Xeon CPU E5506 @ 2.13GHz, 8 cores, 12GB RAM (Linux)
-- 	8000 bodies, 20 iterations
--			Runtimes	Speedup
-- Seq		15.85		1	
-- 1PE		16.95		0.94
-- 2PEs		14.10		1.12
-- 4PEs		11.01		1.44
-- 8PEs		8.54		1.86
  
import IO
import System
import System.Time
import System.Environment
import Data.Int
import Control.Monad hiding (join)
import Debug.Trace
import GHC.Exts
import GHC.Conc (numCapabilities)
--import Control.Seq as Seq
#if defined(STRATEGIES)
#if defined(PARDIST_STRATEGIES)
import Control.Parallel.Strategies
import Future
#endif
#endif
--import Control.Parallel as Seq
import qualified Data.List as List
import qualified Data.Array as Array

--import Future

type Float3D = (Float, Float, Float)

type UFloat3D = (# Float#, Float#, Float# #)

-- mass
g = 9.8
timeStep = 0.001
eps = 0.01

-- This step generates the bodies in the system.
genVector tag = (tag' * 1.0, tag' * 0.2, tag' * 30.0)
   where tag' = fromIntegral tag

-- Only doing the O(N^2) part in parallel:
-- This step computes the accelerations of the bodies.       
compute vecList tag =
       let myvector = vecList Array.! (tag-1) in
       accel myvector vecList
       where --vecList = elems vecArr

--             multTriple :: Float# -> UFloat3D -> UFloat3D
--             multTriple c (# x,y,z #) = (# c*x,c*y,c*z #)

             multTriple :: Float -> Float3D -> Float3D
             multTriple c ( x,y,z ) = ( c*x,c*y,c*z )

-- #define OLD_VER
#ifdef OLD_VER
	     pairWiseAccel :: Float3D -> Float3D -> Float3D
             pairWiseAccel (x,y,z) (x',y',z') = let dx = x'-x
                                                    dy = y'-y
                                                    dz = z'-z
                                                    eps = 0.005
                                                    distanceSq = dx^2 + dy^2 + dz^2 + eps
                                                    factor = 1/sqrt(distanceSq ^ 3)
--                                                in multTriple factor (dx,dy,dz)
                                                in multTriple factor (dx,dy,dz)

             sumTriples = foldr (\(x,y,z) (x',y',z') -> (x+x',y+y',z+z')) (0,0,0)
	     accel vector vecList = multTriple g $ sumTriples $ List.map (pairWiseAccel vector) vecList
#else
-- Making this much leCss haskell like to avoid allocation:
             (strt,end) = Array.bounds vecList

             accel :: Float3D -> (Array.Array Int Float3D) -> Float3D
	     accel vector vecList = 

             -- Manually inlining to see if the tuples unbox:
	        let (# sx,sy,sz #) = loop strt 0 0 0
		    loop !i !ax !ay !az
                      | i == end = (# ax,ay,az #)
		      | otherwise = 
                       let ( x,y,z )    = vector
			   ( x',y',z' ) = vecList Array.! i

                           (# dx,dy,dz #) = (# x'-x, y'-y, z'-z #)
#if 1
			   distanceSq = dx^2 + dy^2 + dz^2 + eps
			   factor = timeStep/sqrt(distanceSq ^ 3)
#else
			   distanceSq = dx*dx + dy*dy + dz*dz + eps
			   factor = timeStep/sqrt(distanceSq * distanceSq * distanceSq)
#endif
			   (# px,py,pz #) = (# factor * dx, factor * dy, factor *dz #)

		       in loop (i+1) (ax+px) (ay+py) (az+pz)
		in ( g*sx, g*sy, g*sz )
#endif


-- This describes the graph-- The same tag collection prescribes the two step collections.             
--run :: Int -> (b, c)
--run :: Int -> ([(Int, (Float, Float, Float))], [(Int, (Float, Float, Float))])
--run :: Int -> ([Float3D], [Float3D])
--run :: Int -> Array Int Float3D -> [Float3D]
run n initVecs = runEval $ do

           -- 10 chunks per Capability
           let chunk = n `quot` (numCapabilities * 30)
#if defined(STRATEGIES)
#if defined(PARDIST_STRATEGIES)
           fs <-{-trace ("Chunksize: " ++ (show chunk))$-} forM [1, 1+chunk .. n] $ \t -> do
                   let t1 = min (t + chunk - 1) n
                   fork (return (withStrategy (evalList rseq) $
                                  map (compute initVecs) [t .. t1]))


#endif 
#else
           fs <-{-trace ("Chunksize: " ++ (show chunk))$-} forM [1, 1+chunk .. n] $ \t -> do
                   let t1 = min (t + chunk - 1) n
                   return ( map (compute initVecs) [t .. t1])

#endif

           ls <- mapM join fs
           return (concat ls)



--seqList :: Strategy a -> Strategy [a]
--seqList _strat []    = ()
--seqList strat (x:xs) = strat x `seq` seqList strat xs

--withStrategy :: Strategy a -> a -> a
--withStrategy = flip using

nSteps n 0 pos vel = (pos,vel)
nSteps n step pos vel = nSteps n (step-1) pos' vel'
	where
		(pos',vel') = oneStep n pos vel

oneStep n pos vel = (new_pos,new_vel)
	where
		new_pos = Array.listArray (0,n-1) $ updatePos n pos new_vel
		new_vel = Array.listArray (0,n-1) $ updateVel n vel accel
		accel = Array.listArray (0,n-1) $ run n pos

updateVel n vel accel = map (\i -> updateV (vel Array.! i) (accel Array.! i)) [0..n-1]
	where
		updateV (vx,vy,vz) (x,y,z) = (vx+x,vy+y,vz+z)

updatePos n pos vel = map (\i -> updateP (pos Array.! i) (vel Array.! i)) [0..n-1]
	where
		updateP (x,y,z) (vx,vy,vz) = ((x + timeStep * vx),(y + timeStep * vy),(z + timeStep * vz))
						
main = 
    do 
		args <- getArgs
		let n = case args of
					[]  -> (3::Int)
					[s] -> (read s)
		let ns = [0..n-1]
		let	initPos = Array.array (0,n-1) [ (i, genVector i) | i <- ns ]
		let	initVel = Array.array (0,n-1) [ (i, genVector (i*n)) | i <- ns ]
		let res1 = (foldl (\sum i -> sum + f (initPos Array.! i) (initVel Array.! i)) 0 ns) -- do meaningless sum...
		print (res1)
		t1 <- getClockTime -- ...so that, at this point, the positions, velocities and masses have been read from file
		let (newPos,newVel) = nSteps n 20 initPos initVel --let	accList = run n initPos
		let res2 = (foldl (\sum i -> sum + f (newPos Array.! i) (newVel Array.! i)) 0 ns) -- do a meaningless sum to generate a small output
		print (res2)
		t2 <- getClockTime
		hPutStrLn stderr ("time taken: " ++ show (secDiff t1 t2) ++ "s")

f (x,y,z) (vx,vy,vz) = x+y+z+vx+vy+vz


-- func to calc time taken between t0 and t1 in sec
secDiff::ClockTime -> ClockTime -> Float
secDiff (TOD secs1 psecs1) (TOD secs2 psecs2) = fromInteger (psecs2 - psecs1) / 1e12 + fromInteger (secs2 - secs1)
