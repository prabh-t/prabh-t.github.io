{-# LANGUAGE BangPatterns #-}
import Array
import Data.List
import System
import System.Time
import IO

data M = M {-# UNPACK #-} !Double
data P = P {-# UNPACK #-} !Double {-# UNPACK #-} !Double {-# UNPACK #-} !Double
data V = V {-# UNPACK #-} !Double {-# UNPACK #-} !Double {-# UNPACK #-} !Double

data Pair = Pair {-# UNPACK #-} !Double {-# UNPACK #-} !Double
data Triple = Triple {-# UNPACK #-} !Double {-# UNPACK #-} !Double {-# UNPACK #-} !Double

data Acc = Acc [(Integer,V)] [(Integer,V)]

n = 512
ns = [0..(n-1)]
arrbounds = (0,(n-1))
toArr xs = listArray arrbounds xs

time_step = 0.001

toDouble s = read s :: Double
word ws i = toDouble (ws!!(fromInteger i))

next s = [a..b] where a = (1024*s)+1; b = (1024*s)+n

main =
	do
		str <- readFile "bodies/bodies.txt" -- positions, velocities and masses for 1024 bodies in file
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
			ps 	= toArr (zip3 x y z)
			pos = toArr $ map (\i -> let (x,y,z) = ps!i in P x y z) ns
			vs 	= toArr (zip3 vx vy vz)
			vel = toArr $ map (\i -> let (vx,vy,vz) = vs!i in V vx vy vz) ns
		let vs' = offsetMomentum vel ms
		print (sum' $ map (\i -> energy i (pos,vs',ms)) ns)
		--t2 <- getClockTime
		let (ps',vs'') = nSteps 20 (pos,vs',ms)
		print (sum' $ map (\i -> energy i (ps',vs'',ms)) ns)
		--t3 <- getClockTime
		--hPutStrLn stderr ("Time taken: " ++ show (secDiff t2 t3) ++ "s") 

nSteps
	:: Int
	-> (Array Integer P, Array Integer V, Array Integer M)
	-> (Array Integer P, Array Integer V)
nSteps 0 (ps,vs,ms) = (ps,vs)
nSteps n (ps,vs,ms) = nSteps (n-1) (oneStep (ps,vs,ms))

oneStep
	:: (Array Integer P, Array Integer V, Array Integer M)
	-> (Array Integer P, Array Integer V, Array Integer M)
oneStep (ps,vs,ms) = (new_ps,new_vs,ms)
	where
		new_ps = updatePosition ps new_vs
		new_vs = accum addChange (array arrbounds updated_vel_i) j_acc_change
		updated_vel_i = fst unz
		j_acc_change = concat $ snd unz
		unz = unzip x
		x = map (updateVel vs ps ms) ns
		
updateVel vs ps ms i = updateVel' ps ms i (i+1) ((i,vs!i),[])
		
updateVel' ps ms i j !acc
	|j == n = acc
	| otherwise = updateVel' ps ms i (j+1) new_acc
		where
			new_acc = ((i,new_i_vel), acc_j_vel_change)
			new_i_vel = deductChange (snd old_i) i_vel_change
			acc_j_vel_change = ((j,j_vel_change):old_acc_j)
			(old_i, old_acc_j) = acc
			i_vel_change = V (dx * mass_j * mag) (dy * mass_j * mag) (dz * mass_j * mag)
			j_vel_change = V (dx * mass_i * mag) (dy * mass_i * mag) (dz * mass_i * mag)
			mag = time_step / (dSquared * distance)
			distance = sqrt (dSquared)
			dSquared = dx*dx + dy*dy + dz*dz + 0.01
			dx = ix - jx
			dy = iy - jy
			dz = iz - jz
			P ix iy iz = ps!i
			P jx jy jz = ps!j
			M mass_i = ms!i
			M mass_j = ms!j		

energy i (ps,vs,ms) = calcEnergy i ps ms e
		where
			!e = 0.5 * (mass_i) * (vx*vx + vy*vy + vz*vz)
			M mass_i = ms!i
			V vx vy vz = vs!i

calcEnergy i ps ms e = e - (sum' $ map (\j -> calcEnergy' i j ps ms) [(i+1)..(n-1)])

calcEnergy' i j ps ms = e
		where
			!e = (mass_i * mass_j) / distance
			M mass_i = ms!i
			M mass_j = ms!j	
			distance = sqrt dSquared
			dSquared = dx*dx + dy*dy + dz*dz + 0.01
			dx = ix - jx
			dy = iy - jy
			dz = iz - jz
			P ix iy iz = ps!i
			P jx jy jz = ps!j

updatePosition ps vs = toArr updated_pos
		where
			updated_pos = map (\i -> updatePos ps vs i) ns --`using` parList rseq

updatePos ps vs i = P new_x new_y new_z
	where
		new_x = x + (time_step * vx)
		new_y = y + (time_step * vy)
		new_z = z + (time_step * vz)
		V vx vy vz = vs!i
		P x y z = ps!i
		
fst3 (a,_,_) = a
snd3 (_,b,_) = b
thrd (_,_,c) = c

sum' = foldl' (+) 0

addChange (V vx1 vy1 vz1) (V vx2 vy2 vz2) = V (vx1+vx2) (vy1+vy2) (vz1+vz2)
deductChange (V vx1 vy1 vz1) (V vx2 vy2 vz2) = V (vx1-vx2) (vy1-vy2) (vz1-vz2)

offsetMomentum::Array Integer V->Array Integer M->Array Integer V
offsetMomentum vs ms = listArray arrbounds new_vs
	where
		new_vs = v1:(tail (elems vs))
		v1 = V (negate (px/m1)) (negate (py/m1)) (negate (pz/m1))
		M m1 = ms!0
		(px,py,pz) = foldl' f (0,0,0) ns --`using` parList rseq
		f (x,y,z) i = (vx*m+x, vy*m+y, vz*m+z)
			where
				V vx vy vz = vs!i
				M m = ms!i

-- func to calc time taken between t0 and t1 in sec
secDiff::ClockTime -> ClockTime -> Float
secDiff (TOD secs1 psecs1) (TOD secs2 psecs2) = fromInteger (psecs2 - psecs1) / 1e12 + fromInteger (secs2 - secs1)
