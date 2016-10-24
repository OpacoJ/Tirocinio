module Matrix where

import Data.List
import System.Random
import System.IO
import Control.Parallel
import Control.Parallel.Strategies
	
type Vector = [Double]
type Matrix = [Vector]	

makeMat :: Int -> Matrix
makeMat n = replicate n [1.0..t] where t = fromIntegral n

zeroes n = replicate n (replicate n 0.0)

identity :: Int -> Matrix
identity n = identity2 n 0

identity2 :: Int -> Int -> Matrix
identity2 n m 
	| n == m = []
	| otherwise = [((replicate m 0) ++ [1] ++ (replicate (n-m-1) 0))] ++ (identity2 n (m+1))  

--operazioni tra vettore
--prodotto vettore x scalare
smul :: Double -> Vector -> Vector
smul val = map (val*)
--somma
sumVector :: Vector -> Vector -> Vector
sumVector = zipWith (+)

--differenza
subVector :: Vector -> Vector -> Vector
subVector = zipWith (-)

--prodotto scalare					  
scalar :: Vector -> Vector -> Double
scalar a b = sum (zipWith (*) a b)


--operazioni sequenziali tra matrici

--somma 
sumMatrix :: Matrix -> Matrix -> Matrix
sumMatrix = zipWith (zipWith (+))

--differenza
subMatrix :: Matrix -> Matrix -> Matrix
subMatrix = zipWith (zipWith (-))

--prodotto
prodMatrix :: Matrix -> Matrix -> Matrix
prodMatrix m1 m2 = [[scalar a b | b <- column] | a <- m1]
                 where column = transpose m2

--potenza
powMatrix :: Matrix -> Int -> Matrix
powMatrix m 0 = (identity dim) where dim = length m
powMatrix m 1 = m
powMatrix m n = prodMatrix m (powMatrix m (n-1))
	
	
--operazioni parallele tra matrici

--somma	
sumMatPar :: Matrix -> Matrix -> Matrix
sumMatPar a b = (sumMatrix a b) `using` parList rdeepseq

--differenza
subMatPar :: Matrix -> Matrix -> Matrix
subMatPar a b = (subMatrix a b) `using` parList rdeepseq
--prodotto
prodMatPar :: Matrix -> Matrix -> Matrix
prodMatPar a b = (prodMatrix a b) `using` parList rdeepseq
--potenza	  
powMatPar :: Matrix -> Int -> Matrix
powMatPar m 0 = (identity dim) where dim = length m
powMatPar m 1 = m
powMatPar m n = prodMatPar m ris
				where
					ris = powMatPar m (n-1)

				  
--determinante

--metodo di Laplace
det :: Matrix -> Double
det [] = error "Error input Matrix"
det [[x]] = x
det m = sum [a*s*(det m1) | i <- [0..dim-1], let a = (head m !! i), let m1 = (minor m i 0), let s = (-1)^i] 
	where
		dim = length m
	
deleteElement :: [a] -> Int -> [a]
deleteElement x index = left ++ right where (left, _:right) = splitAt index x

deleteRow :: Matrix -> Int -> Matrix
deleteRow [] _ = error "Error input Matrix"
deleteRow m row = [deleteElement x row | x <- m]
						

minor :: Matrix -> Int -> Int -> Matrix
minor [] _ _ = error "Error input Matrix"
minor m row col = deleteRow m1 row where m1 = (deleteElement m col)




--eliminazione gaussiana		
gauss :: Matrix -> Matrix
gauss mat 
	| length mat > 1 = eliminate mat
    | otherwise      = mat

eliminate :: Matrix -> Matrix
eliminate (x:xs) = x : (map (0:) (gauss (map (reduce x) xs)))	


reduce :: Vector -> Vector -> Vector
reduce (l:lr) (r:rr) = subVector rr (smul (aux r l) lr) where
		aux	0 0 = 0
		aux x y = x / y
		
det2 :: Matrix -> Double
det2 [] = error "Error input Matrix"
det2 m = product [m1 !! i !! i | i <- [0..dim-1]]
	where
		dim = length m
		m1 = gauss m

--determinante parallelo

--metodo di Laplace	
detList :: Matrix -> Vector
detList [] = error "Error input Matrix"
detList [[x]] = [x]
detList m = [a*s*(det m1) | i <- [0..dim-1], let a = (head m !! i), let m1 = (minor m i 0), let s = (-1)^i] 
	where
		dim = length m
		

pardet :: Matrix -> Double
pardet m = sum ((detList m) `using` parList rpar)


--eliminazione gaussiana
gauss' :: Matrix -> Matrix
gauss' mat 
	| length mat > 1 = eliminate' mat
    | otherwise      = mat

eliminate' :: Matrix -> Matrix
eliminate' (x:xs) = x : (map (0:) (gauss (map (reduce x) xs)) `using` parList rseq)

pardet2 :: Matrix -> Double
pardet2 m = product [m1 !! i !! i | i <- [0..dim-1]]
	where
		dim = length m
		m1 = gauss' m
		
		
		
--matrice inversa (determinante NON efficiente)
invert :: Matrix -> Matrix
invert [[]] = [[]]
invert m = transpose [[s*(det m1)/d | i <- [0..dim-1], let m1 = (minor m i j), let s = (-1)^(i+j)] | j <- [0..dim-1]]
		where
			dim = length m
			d = det m

--matrice inversa (determinante efficiente)		
invert2 :: Matrix -> Matrix
invert2 [[]] = [[]]
invert2 m = transpose [[s*(det m1)/d | i <- [0..dim-1], let m1 = (minor m i j), let s = (-1)^(i+j)] | j <- [0..dim-1]]
		where
			dim = length m
			d = det m
			
			
--matrice inversa parallela	(determinante NON efficiente)			
parinvert :: Matrix -> Matrix			
parinvert m = (invert m) `using` parList rdeepseq


--matrice inversa parallela	(determinante NON efficiente)	
parinvert2 :: Matrix -> Matrix			
parinvert2 m = (invert2 m) `using` parList rdeepseq
