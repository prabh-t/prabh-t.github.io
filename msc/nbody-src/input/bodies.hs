type Pos = (Double,Double,Double)
type Vel = (Double,Double,Double)
type Mass = Double

type Body = (Pos,Vel,Mass)

n::Int
n = 1024

toDouble::String->Double
toDouble s = read s :: Double

readbodies = do
	str <- readFile "d:/bodies.txt"
	let ws = words str
	let nb = ws!!0 -- num of bodies, todo-check if nb==1024
	let m = [toDouble (ws!!i)|i<-[(n*0)+1..(n*1)]]
	let x = [toDouble (ws!!i)|i<-[(n*1)+1..(n*2)]]
	let y = [toDouble (ws!!i)|i<-[(n*2)+1..(n*3)]]
	let z = [toDouble (ws!!i)|i<-[(n*3)+1..(n*4)]]
	let vx = [toDouble (ws!!i)|i<-[(n*4)+1..(n*5)]]
	let vy = [toDouble (ws!!i)|i<-[(n*5)+1..(n*6)]]
	let vz = [toDouble (ws!!i)|i<-[(n*6)+1..(n*7)]]
	let xyz = zip3 x y z
	let vxvyvz = zip3 vx vy vz
	let bodies = zip3 xyz vxvyvz m
	let newStr = foldl appendStr "" bodies
	writeFile "d:/bodies2.txt" newStr
	return ""

appendStr str (p,v,m) = str ++ mS ++ " " ++ pS ++ " " ++ vS ++ "\n"
	where
		mS = (show m)
		pS = (show (fst3 p)) ++ " " ++ (show (snd3 p)) ++ " " ++ (show (thrd p))
		vS = (show (fst3 v)) ++ " " ++ (show (snd3 v)) ++ " " ++ (show (thrd v))

fst3 (x,_,_) = x
snd3 (_,x,_) = x
thrd (_,_,x) = x
				
test bs =
		do
			str <- readFile "d:/bodies.txt"
			let ws = words str
			let xs = [toDouble (ws!!i)|i<-[2..11]]
			return (map (\x->x+1) xs)