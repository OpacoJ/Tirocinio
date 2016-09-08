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

--operazioni sequenziali tra matrici

sumVector :: Vector -> Vector -> Vector
sumVector v1 v2 = [a + b | a <- v1, b <- v2]

subVector :: Vector -> Vector -> Vector
subVector v1 v2 = [a - b | a <- v1, b <- v2]
					  
scalar :: Vector -> Vector -> Double
scalar a b = sum (zipWith (*) a b)


sumMatrix :: Matrix -> Matrix -> Matrix
sumMatrix = zipWith (zipWith (+))

subMatrix :: Matrix -> Matrix -> Matrix
subMatrix = zipWith (zipWith (-))

prodMatrix :: Matrix -> Matrix -> Matrix
prodMatrix m1 m2 = [[scalar a b | b <- column] | a <- m1]
                 where column = transpose m2

powMatrix :: Matrix -> Int -> Matrix
powMatrix m 0 = (identity dim) where dim = length m
powMatrix m 1 = m
powMatrix m n = prodMatrix m (powMatrix m (n-1))
	
	
--operazioni parallele tra matrici
	
sumMatPar :: Matrix -> Matrix -> Matrix
sumMatPar a b = (sumMatrix a b) `using` parList rdeepseq

subMatPar :: Matrix -> Matrix -> Matrix
subMatPar a b = (subMatrix a b) `using` parList rdeepseq
	  
prodMatPar :: Matrix -> Matrix -> Matrix
prodMatPar a b = (prodMatrix a b) `using` parList rdeepseq


				  
powMatPar :: Matrix -> Int -> Matrix
powMatPar m 0 = (identity dim) where dim = length m
powMatPar m 1 = m
powMatPar m n = prodMatPar m ris
				where
					ris = powMatPar m (n-1)

				  
--determinante

deleteColumn :: Matrix -> Int -> Matrix
deleteColumn [] _ = error "Error input Matrix"
deleteColumn m col = a ++ b where (a, _:b) = splitAt col m
	
deleteElement :: Vector -> Int -> Vector
deleteElement x index = left ++ right where (left, _:right) = splitAt index x

deleteRow :: Matrix -> Int -> Matrix
deleteRow [] _ = error "Error input Matrix"
deleteRow m row = [deleteElement x row | x <- m]
						

minor :: Matrix -> Int -> Int -> Matrix
minor [] _ _ = error "Error input Matrix"
minor m row col = deleteRow m1 row where m1 = (deleteColumn m col)

det :: Matrix -> Double
det [] = error "Error input Matrix"
det [[x]] = x
det m = sum [a*s*(det m1) | i <- [0..dim-1], let a = (head m !! i), let m1 = (minor m i 0), let s = (-1)^i] 
	where
		dim = length m
		
--determinante parallelo

		
detList :: Matrix -> Vector
detList [] = error "Error input Matrix"
detList [[x]] = [x]
detList m = [a*s*(det m1) | i <- [0..dim-1], let a = (head m !! i), let m1 = (minor m i 0), let s = (-1)^i] 
	where
		dim = length m
		
pardet :: Matrix -> Double
pardet m = sum ((detList m) `using` parList rpar)
		
--matrice inversa

invert :: Matrix -> Matrix
invert [[]] = [[]]
invert m = transpose [[s*(det m1)/d | i <- [0..dim-1], let m1 = (minor m i j), let s = (-1)^(i+j)] | j <- [0..dim-1]]
		where
			dim = length m
			d = det m

--matrice inversa parallela			
			
parinvert :: Matrix -> Matrix			
parinvert m = (invert m) `using` parList rdeepseq