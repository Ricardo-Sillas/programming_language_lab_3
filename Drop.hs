module Drop where
	import Prelude hiding(drop)
	drop [] n = []
	drop [_] n = [n]
	drop (x:y:z) n 
		| (n /= 1 && n/= 2) = (x:y:z)
		| (y==0) = x: drop (y:z) n
		| otherwise = (n:y:z)