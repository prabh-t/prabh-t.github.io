-- Prabhat
-- Adapted Java implementation in Haskell
-- Initial version (5 bodies, time step 0.01)

solar_mass = 4 * pi * pi
days_per_year = 365.24
time_step = 0.01

-- ((x, y, z), (vx, vy, vz), mass)
type Body = ((Double,Double,Double),(Double,Double,Double),Double)

energy::[Body]->Double
energy bs = inc_e bs - sum ij
        where
                ij = [dece (i,j) bs|i<-[0..last_index],j<-[(i+1)..last_index]]
                last_index = (length bs)-1
                
dece::(Int,Int)->[Body]->Double
dece (i,j) [] = 0
dece (i,j) bs = (getmass b_i * getmass b_j) / distance
        where
                distance = sqrt dSquared
                dSquared = dx^2 + dy^2 + dz^2 + 0.01
                dx = (getx b_i) - (getx b_j)
                dy = (gety b_i) - (gety b_j)
                dz = (getz b_i) - (getz b_j)
                b_i = bs!!i
                b_j = bs!!j

inc_e::[Body]->Double
inc_e [] = 0
inc_e (b:bs) = e + inc_e bs
                where
                        e = 0.5 * (getmass b) * ((getvx b)^2 + (getvy b)^2 + (getvz b)^2)
                                

advanceN::Int->[Body]->[Body]
advanceN 0 bs = bs
advanceN n bs = advanceN (n-1) (advance bs)

advance::[Body]->[Body]
advance bs = fg (ff ij bs)
        where 
                ij = [(i,j)|i<-[0..last_index],j<-[(i+1)..last_index]]          
                        where
                                last_index = (length bs)-1
                                
fg::[Body]->[Body]
fg [] = []
fg (b:bs) = ((new_x, new_y, new_z),(getvx b, getvy b, getvz b),getmass b):(fg bs)
        where
                new_x = (getx b) + (time_step * (getvx b))
                new_y = (gety b) + (time_step * (getvy b))
                new_z = (getz b) + (time_step * (getvz b))
                                
ff::[(Int,Int)]->[Body]->[Body]
ff [x] bs = update bs x
ff (x:xs) bs = ff xs updatedList
        where
                updatedList = update bs x

update::[Body]->(Int,Int)->[Body]
update bs (i,j) = updateList (updateList bs i (fst new_b_ij)) j (snd new_b_ij)
        where
                new_b_ij = updateBs (b_i,b_j)
                b_i = bs!!i
                b_j = bs!!j
                
-- takes old list, index to update, new value
updateList::[Body]->Int->Body->[Body]
updateList [] _ _ = []
updateList l i v = (take i l)++[v]++(drop (i+1) l)

updateBs::(Body,Body)->(Body,Body)
updateBs (i,j) = ((getxyz i, i_vxvyvz, getmass i),(getxyz j, j_vxvyvz, getmass j))
                where
                        i_vxvyvz = (i_vx, i_vy, i_vz)
                        j_vxvyvz = (j_vx, j_vy, j_vz)
                        i_vx = (getvx i) - (dx * getmass j * mag)
                        i_vy = (getvy i) - (dy * getmass j * mag)
                        i_vz = (getvz i) - (dz * getmass j * mag)
                        j_vx = (getvx j) + (dx * getmass i * mag)
                        j_vy = (getvy j) + (dy * getmass i * mag)
                        j_vz = (getvz j) + (dz * getmass i * mag)
                        mag = time_step / (dSquared * distance)
                        distance = sqrt dSquared
                        dSquared = dx^2 + dy^2 + dz^2 + 0.01
                        dx = (getx i) - (getx j)
                        dy = (gety i) - (gety j)
                        dz = (getz i) - (getz j)
  
getbodies::[Body]
getbodies = (offsetMomentum (pxpypz bodies) (head bodies)):(tail bodies)
                        where bodies = initialBodies
                        
          
pxpypz::[Body]->(Double,Double,Double)
pxpypz bs = foldr f (0,0,0) [((getvx b * getmass b), (getvy b * getmass b), (getvz b * getmass b ))|b<-bs]
        where
                f (a,b,c) (x,y,z) = (a+x, b+y, c+z)

offsetMomentum::(Double,Double,Double)->Body->Body
offsetMomentum (px,py,pz) b = (getxyz b, ((negate px) / solar_mass, (negate py) / solar_mass, (negate pz) / solar_mass), getmass b)

getxyz::Body->(Double,Double,Double)
getxyz (xyz,_,_) = xyz

getx::Body->Double
getx ((x,_,_),_,_) = x
gety::Body->Double
gety ((_,y,_),_,_) = y
getz::Body->Double
getz ((_,_,z),_,_) = z

getvx::Body->Double
getvx (_,(vx,_,_),_) = vx
getvy::Body->Double
getvy (_,(_,vy,_),_) = vy
getvz::Body->Double
getvz (_,(_,_,vz),_) = vz

getmass::Body->Double
getmass (_,_,m) = m

initialBodies::[Body]
initialBodies =
        [((0, 0, 0), (0, 0, 0), solar_mass)
        ,((4.84143144246472090e+00, -1.16032004402742839e+00, -1.03622044471123109e-01), (1.66007664274403694e-03 * days_per_year, 7.69901118419740425e-03 * days_per_year, -6.90460016972063023e-05 * days_per_year), 9.54791938424326609e-04 * solar_mass)
        ,((8.34336671824457987e+00, 4.12479856412430479e+00, -4.03523417114321381e-01), (-2.76742510726862411e-03 * days_per_year, 4.99852801234917238e-03 * days_per_year, 2.30417297573763929e-05 * days_per_year), 2.85885980666130812e-04 * solar_mass)
    ,((1.28943695621391310e+01, -1.51111514016986312e+01, -2.23307578892655734e-01), (2.96460137564761618e-03 * days_per_year, 2.37847173959480950e-03 * days_per_year, -2.96589568540237556e-05 * days_per_year), 4.36624404335156298e-05 * solar_mass)
    ,((1.53796971148509165e+01, -2.59193146099879641e+01, 1.79258772950371181e-01), (2.68067772490389322e-03 * days_per_year, 1.62824170038242295e-03 * days_per_year, -9.51592254519715870e-05 * days_per_year), 5.15138902046611451e-05 * solar_mass)]
        