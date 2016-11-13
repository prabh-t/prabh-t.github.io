{-
 - Modified from code released with:
 - Intel Concurrent Collections for Haskell
 - Copyright (c) 2010, Intel Corporation.
 -}
{-# OPTIONS -fglasgow-exts #-}
{-# LANGUAGE ExistentialQuantification
   , ScopedTypeVariables
   , BangPatterns
   , NamedFieldPuns 
   , RecordWildCards
   , FlexibleInstances
   , DeriveDataTypeable
   , MagicHash 
   , CPP
  #-}
-- This is INCOMPATIBLE with CncPure..

-- Author: Chih-Ping Chen
-- Ported to Monad-par by Ryan Newton.

-- This program uses CnC to calculate the accelerations of the bodies in a 3D system.  
  
import Control.Monad.Par
-- import Par
import Control.Monad
import Data.Int
import qualified Data.List as List
import qualified Data.Array as A
import GHC.Exts
import System.Environment

type Float3D = (Float, Float, Float)
type UFloat3D = (# Float#, Float#, Float# #)


-- This step generates the bodies in the system.
genVector tag = (tag' * 1.0, tag' * 0.2, tag' * 30.0)
   where tag' = fromIntegral tag

-- We are keeping the idiomatic Haskell version around as well for comparison: 
-- #define IDIOMATIC_VER

-- Only doing the O(N^2) part in parallel:
-- This step computes the accelerations of the bodies.       
compute :: A.Array Int Float3D -> A.Array Int (IVar Float3D) -> Int -> Par ()
compute vecList accels tag =
    do 
       let myvector = vecList A.! (tag-1)
       put (accels A.! tag) (accel myvector vecList)
       where 
             g = 9.8

             multTriple :: Float -> Float3D -> Float3D
             multTriple c ( x,y,z ) = ( c*x,c*y,c*z )

	     pairWiseAccel :: Float3D -> Float3D -> Float3D
             pairWiseAccel (x,y,z) (x',y',z') = let dx = x'-x
                                                    dy = y'-y
                                                    dz = z'-z
                                                    eps = 0.005
						    -- Performance degredation here:
						    distanceSq = dx*dx + dy*dy + dz*dz + eps
						    factor = 1/sqrt(distanceSq * distanceSq * distanceSq)

--                                                in multTriple factor (dx,dy,dz)
                                                in multTriple factor (dx,dy,dz)
#ifdef IDIOMATIC_VER
             sumTriples = foldr (\(x,y,z) (x',y',z') -> (x+x',y+y',z+z')) (0,0,0)
	     accel vector vecList = multTriple g $ sumTriples $ List.map (pairWiseAccel vector) vecList
#else
-- Making this much less idiomatic to avoid allocation:
             (strt,end) = A.bounds vecList

             accel :: Float3D -> (A.Array Int Float3D) -> Float3D
	     accel vector vecList = 

             -- Manually inlining to see if the tuples unbox:
	        let (# sx,sy,sz #) = loop strt 0 0 0
		    loop !i !ax !ay !az
                      | i == end = (# ax,ay,az #)
		      | otherwise = 
                       let ( x,y,z )    = vector
			   ( x',y',z' ) = vecList A.! i

                           (# dx,dy,dz #) = (# x'-x, y'-y, z'-z #)
			   eps = 0.005
			   distanceSq = dx*dx + dy*dy + dz*dz + eps
			   factor = 1/sqrt(distanceSq * distanceSq * distanceSq)

			   (# px,py,pz #) = (# factor * dx, factor * dy, factor *dz #)

		       in loop (i+1) (ax+px) (ay+py) (az+pz)
		in ( g*sx, g*sy, g*sz )
#endif


--run :: Int -> [Float3D]
--run n = runPar $ 
run n initVecs = runPar $
        do 
	   vars <- sequence$ take n $ repeat new
--           accels  <- A.array (0,n-1) [ (i,) | i <- [0..n-1]]
	   -- Is there a better way to make an array of pvars?
           let accels = A.array (1,n) (zip [1..n] vars)

#ifdef IDIOMATIC_VER
           --let initVecs = List.map genVector [1..n]
#else
           --let initVecs = A.array (0,n-1) [ (i, genVector i) | i <- [0..n-1] ]
#endif
           
	   forM_ [1..n] $ \ t -> fork (compute initVecs accels t)

           sequence (List.map (\i -> get (accels A.! i)) [1..n])

	  
nSteps n 0 pos vel = (pos,vel)
nSteps n step pos vel = nSteps n (step-1) pos' vel'
	where
		(pos',vel') = oneStep n pos vel

oneStep n pos vel = (new_pos,new_vel)
	where
		new_pos = Array.listArray (0,n-1) $ updatePos n chunksize pos new_vel
		new_vel = Array.listArray (0,n-1) $ updateVel n chunksize vel accel
		accel = Array.listArray (0,n-1) $ run n pos
		chunksize = n `quot` (numCapabilities * 2)

updateVel n chunksize vel accel = map (\i -> updateV (vel Array.! i) (accel Array.! i)) [0..n-1] `using` parListChunk chunksize rdeepseq 
	where
		updateV (vx,vy,vz) (x,y,z) = (vx+x,vy+y,vz+z)

updatePos n chunksize pos vel = map (\i -> updateP (pos Array.! i) (vel Array.! i)) [0..n-1] `using` parListChunk chunksize rdeepseq 
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
