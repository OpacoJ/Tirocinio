module Matrix where

import Data.List
import System.Random
import System.IO
import Control.Parallel
import Control.Parallel.Strategies
	
makeMat :: Int -> [[Double]]
makeMat n = replicate n [1.0..t] where t = fromIntegral n

zeroes n = replicate n (replicate n 0.0)

identity :: Int -> [[Double]]
identity n = identity2 n 0

identity2 :: Int -> Int -> [[Double]]
identity2 n m 
	| n == m = []
	| otherwise = [((replicate m 0) ++ [1] ++ (replicate (n-m-1) 0))] ++ (identity2 n (m+1))  

--operazioni sequenziali tra matrici

sumVector :: [Double] -> [Double] -> [Double]
sumVector v1 v2 = [a + b | a <- v1, b <- v2]

subVector :: [Double] -> [Double] -> [Double]
subVector v1 v2 = [a - b | a <- v1, b <- v2]
					  
scalar :: [Double] -> [Double] -> Double
scalar a b = sum (zipWith (*) a b)


sumMatrix :: [[Double]] -> [[Double]] -> [[Double]]
sumMatrix = zipWith (zipWith (+))

subMatrix :: [[Double]] -> [[Double]] -> [[Double]]
subMatrix = zipWith (zipWith (-))

prodMatrix :: [[Double]] -> [[Double]] -> [[Double]]
prodMatrix m1 m2 = [[scalar a b | b <- column] | a <- m1]
                 where column = transpose m2

powMatrix :: [[Double]] -> Int -> [[Double]]
powMatrix m 0 = (identity dim) where dim = length m
powMatrix m 1 = m
powMatrix m n = prodMatrix m (powMatrix m (n-1))
	
	
--operazioni parallele tra matrici
	
sumMatPar :: [[Double]] -> [[Double]] -> [[Double]]
sumMatPar a b = (sumMatrix a b) `using` parList rdeepseq

subMatPar :: [[Double]] -> [[Double]] -> [[Double]]
subMatPar a b = (subMatrix a b) `using` parList rdeepseq
	  
prodMatPar :: [[Double]] -> [[Double]] -> [[Double]]
prodMatPar a b = (prodMatrix a b) `using` parList rdeepseq


				  
powMatPar :: [[Double]] -> Int -> [[Double]]
powMatPar m 0 = (identity dim) where dim = length m
powMatPar m 1 = m
powMatPar m n = prodMatPar m ris
				where
					ris = powMatPar m (n-1)

				  
--determinante

deleteColumn :: [[Double]] -> Int -> [[Double]]
deleteColumn [] _ = error "Error input Matrix"
deleteColumn m col = a ++ b where (a, _:b) = splitAt col m
	
deleteElement :: [Double] -> Int -> [Double]
deleteElement x index = left ++ right where (left, _:right) = splitAt index x

deleteRow :: [[Double]] -> Int -> [[Double]]
deleteRow [] _ = error "Error input Matrix"
deleteRow m row = [deleteElement x row | x <- m]
						

minor :: [[Double]] -> Int -> Int -> [[Double]]
minor [] _ _ = error "Error input Matrix"
minor m row col = deleteRow m1 row where m1 = (deleteColumn m col)

det :: [[Double]] -> Double
det [] = error "Error input Matrix"
det [[x]] = x
det m = sum [a*s*(det m1) | i <- [0..dim-1], let a = (head m !! i), let m1 = (minor m i 0), let s = (-1)^i] 
	where
		dim = length m
		
--determinante parallelo

		
detList :: [[Double]] -> [Double]
detList [] = error "Error input Matrix"
detList [[x]] = [x]
detList m = [a*s*(det m1) | i <- [0..dim-1], let a = (head m !! i), let m1 = (minor m i 0), let s = (-1)^i] 
	where
		dim = length m
		
pardet :: [[Double]] -> Double
pardet m = sum ((detList m) `using` parList rpar)
		
--matrice inversa

invert :: [[Double]] -> [[Double]]
invert [[]] = [[]]
invert m = transpose [[s*(det m1)/d | i <- [0..dim-1], let m1 = (minor m i j), let s = (-1)^(i+j)] | j <- [0..dim-1]]
		where
			dim = length m
			d = det m
		
		