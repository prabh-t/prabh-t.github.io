import Array
import Data.List
import Control.Parallel 
import Control.Parallel.Strategies
import Control.DeepSeq
import GHC.Exts
import GHC.Conc (numCapabilities)
import System
import System.Time
import IO

n = 1024

time_step = 0.001

toDouble s = read s :: Double
word ws i = toDouble (ws!!(fromInteger i))

main =
	do
		str <- readFile "bodies/bodies.txt" -- positions, velocities and masses for 1024 bodies in file
		let 
			ws 	= words str
			nb 	= ws!!0 -- num of bodies
			ms 	= listArray (0,(n-1)) [word ws i|i<-[ms1..ms2]] where ms1 = (1024*0)+1; ms2 = (1024*0)+n
			x 	= [word ws i|i<-[x1..x2]] where x1 = (1024*1)+1; x2 = (1024*1)+n
			y 	= [word ws i|i<-[y1..y2]] where y1 = (1024*2)+1; y2 = (1024*2)+n
			z 	= [word ws i|i<-[z1..z2]] where z1 = (1024*3)+1; z2 = (1024*3)+n
			vx 	= [word ws i|i<-[vx1..vx2]] where vx1 = (1024*4)+1; vx2 = (1024*4)+n
			vy 	= [word ws i|i<-[vy1..vy2]] where vy1 = (1024*5)+1; vy2 = (1024*5)+n
			vz 	= [word ws i|i<-[vz1..vz2]] where vz1 = (1024*6)+1; vz2 = (1024*6)+n
			ps 	= listArray (0,(n-1)) (zip3 x y z)
			vs 	= listArray (0,(n-1)) (zip3 vx vy vz)
		let vs' = offsetMomentum vs ms
		print (sum [energy i (ps,vs',ms)|i<-[0..(n-1)]])
		t2 <- getClockTime
		let (ps',vs'') = nSteps 20 (ps,vs',ms)
		print (sum [energy i (ps',vs'',ms)|i<-[0..(n-1)]])
		t3 <- getClockTime
		hPutStrLn stderr ("Time taken: " ++ show (secDiff t2 t3) ++ "s") 

nSteps 0 (ps,vs,ms) = (ps,vs)
nSteps n (ps,vs,ms) = nSteps (n-1) (oneStep (ps,vs,ms))
		
oneStep (ps,vs,ms) = (new_ps,new_vs,ms)
	where
		new_ps = updatePosition ps new_vs
		new_vs = accum addChange (array (0,(n-1)) updated_vel_i) j_acc_change
		updated_vel_i = fst unz
		j_acc_change = concat $ snd unz
		unz = unzip x
		x = concat $ y
		y = withStrategy (evalList rseq) $ map (\ts -> updateSegment ts vs ps ms) segments
		--y = map (\t -> segment t chunk vs ps ms) [1, 1+chunk .. n] `using` parList rseq
		segments = map (\t -> [t..(min (t + chunksize - 1) n)]) [1,1+chunksize .. n]
		chunksize = n `quot` (toInteger numCapabilities * 30)
		--chunk = n `quot` (toInteger numCapabilities * 30)

updateSegment ts vs ps ms = withStrategy (evalList rseq) $ map (updateVel vs ps ms) ts
		
segment t chunk vs ps ms = map (updateVel vs ps ms) [t..t1] `using` parList rseq
	where
		t1 = min (t + chunk - 1) n

updateVel vs ps ms t = updateVel' ps ms i j ((i,vs!i),[])
	where
		i = (t-1)
		j = (i+1)

updateVel' ps ms i j !acc
	|j == n = acc
	| otherwise = updateVel' ps ms i (j+1) new_acc
		where
			new_acc = ((i,new_i_vel), acc_j_vel_change)
			new_i_vel = deductChange (snd old_i) i_vel_change
			acc_j_vel_change = ((j,j_vel_change):old_acc_j)
			(old_i, old_acc_j) = acc
			i_vel_change = ((dx * mass_j * mag), (dy * mass_j * mag), (dz * mass_j * mag))
			j_vel_change = ((dx * mass_i * mag), (dy * mass_i * mag), (dz * mass_i * mag))
			mag = time_step / (dSquared * distance)
			distance = sqrt (dSquared)
			dSquared = dx*dx + dy*dy + dz*dz + 0.01
			dx = (fst3 pos_i) - (fst3 pos_j)
			dy = (snd3 pos_i) - (snd3 pos_j)
			dz = (thrd pos_i) - (thrd pos_j)
			pos_i = ps!i
			pos_j = ps!j
			mass_i = ms!i
			mass_j = ms!j		

energy i (ps,vs,ms) = calcEnergy i ps ms e
		where
			!e = 0.5 * (ms!i) * (vx*vx + vy*vy + vz*vz)
			vx = fst3 (vs!i)
			vy = snd3 (vs!i)
			vz = thrd (vs!i)

calcEnergy i ps ms e = e - sum [calcEnergy' i j ps ms|j<-[(i+1)..(n-1)]]

calcEnergy' i j ps ms = e
		where
			!e = (ms!i * ms!j) / distance
			distance = sqrt dSquared
			dSquared = dx*dx + dy*dy + dz*dz + 0.01
			dx = ix - jx
			dy = iy - jy
			dz = iz - jz
			ix = fst3 (ps!i)
			iy = snd3 (ps!i)
			iz = thrd (ps!i)
			jx = fst3 (ps!j)
			jy = snd3 (ps!j)
			jz = thrd (ps!j)

updatePosition ps vs = listArray (0,(n-1)) updated_pos
		where
			updated_pos = map (updatePos ps vs) [i|i<-[0..(n-1)]] `using` parList rseq

updatePos ps vs i = (new_x, new_y, new_z)
	where
		new_x = (fst3 p) + (time_step * (fst3 v))
		new_y = (snd3 p) + (time_step * (snd3 v))
		new_z = (thrd p) + (time_step * (thrd v))
		v = vs!i
		p = ps!i
		
fst3 (a,_,_) = a
snd3 (_,b,_) = b
thrd (_,_,c) = c

addChange (vx1,vy1,vz1) (vx2,vy2,vz2) = (vx1+vx2,vy1+vy2,vz1+vz2)

deductChange (vx1,vy1,vz1) (vx2,vy2,vz2) = (vx1-vx2,vy1-vy2,vz1-vz2)

offsetMomentum vs ms = listArray (0,(n-1)) new_vs
	where
		new_vs = new_v:(tail (elems vs))
		new_v = (vx,vy,vz)
		vx = (negate (fst3 (pxpypz vs ms))) / m
		vy = (negate (snd3 (pxpypz vs ms))) / m
		vz = (negate (thrd (pxpypz vs ms))) / m
		m = ms!0
          
pxpypz vs ms = foldl' f (0,0,0) (map veloMultMass [i|i<-[0..(n-1)]] `using` parList rseq)
        where
			f (a,b,c) (x,y,z) = (a+x, b+y, c+z)
			veloMultMass i = ((fst3 v * m), (snd3 v * m), (thrd v * m))
				where
					v = vs!i
					m = ms!i

-- func to calc time taken between t0 and t1 in sec
secDiff::ClockTime -> ClockTime -> Float
secDiff (TOD secs1 psecs1) (TOD secs2 psecs2) = fromInteger (psecs2 - psecs1) / 1e12 + fromInteger (secs2 - secs1)
