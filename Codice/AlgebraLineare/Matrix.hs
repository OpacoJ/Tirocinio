module Matrix where

import Data.List
import System.Random
import System.IO
import Control.Parallel
import Control.Parallel.Strategies
	

makeMat n = replicate n [1..n]

zeroes n = replicate n (replicate n 0)

identity :: Num a => Int -> [[a]]
identity n = identity2 n 0

identity2 :: Num a => Int -> Int -> [[a]]

identity2 n m 
	| n == m = []
	| otherwise = [((replicate m 0) ++ [1] ++ (replicate (n-m-1) 0))] ++ (identity2 n (m+1))  

--operazioni sequenziali tra matrici

sumVector :: Num a => [a] -> [a] -> [a]
sumVector v1 v2 = [a + b | a <- v1, b <- v2]

subVector :: Num a => [a] -> [a] -> [a]
subVector v1 v2 = [a - b | a <- v1, b <- v2]
					  
scalar :: Num a => [a] -> [a] -> a
scalar a b = sum (zipWith (*) a b)


sumMatrix :: Num a => [[a]] -> [[a]] -> [[a]]
sumMatrix = zipWith (zipWith (+))

subMatrix :: Num a => [[a]] -> [[a]] -> [[a]]
subMatrix = zipWith (zipWith (-))

prodMatrix :: Num a => [[a]] -> [[a]] -> [[a]]
prodMatrix m1 m2 = [[scalar a b | b <- column] | a <- m1]
                 where column = transpose m2

powMatrix :: Num a => [[a]] -> Int -> [[a]]
powMatrix m 0 = (identity dim) where dim = length m
powMatrix m 1 = m
powMatrix m n = prodMatrix m (powMatrix m (n-1))
	
	
--operazioni parallele tra matrici
	
sumMatPar :: (NFData a, Num a) => [[a]] -> [[a]] -> [[a]]
sumMatPar a b = (sumMatrix a b) `using` parList rdeepseq

subMatPar :: (NFData a, Num a) => [[a]] -> [[a]] -> [[a]]
subMatPar a b = (subMatrix a b) `using` parList rdeepseq
	  
prodMatPar :: (NFData a, Num a) => [[a]] -> [[a]] -> [[a]]
prodMatPar a b = (prodMatrix a b) `using` parList rdeepseq


				  
powMatPar :: (NFData a, Num a) => [[a]] -> Int -> [[a]]
powMatPar m 0 = (identity dim) where dim = length m
powMatPar m 1 = m
powMatPar m n = prodMatPar m ris
				where
					ris = powMatPar m (n-1)

				  
--determinante

deleteColumn :: [[a]] -> Int -> [[a]]
deleteColumn [] _ = error "Error input Matrix"
deleteColumn m col = a ++ b where (a, _:b) = splitAt col m
	
deleteElement :: [a] -> Int -> [a]
deleteElement x index = left ++ right where (left, _:right) = splitAt index x

deleteRow :: [[a]] -> Int -> [[a]]
deleteRow [] _ = error "Error input Matrix"
deleteRow m row = [deleteElement x row | x <- m]
						

minor :: (Num a, Fractional a) => [[a]] -> Int -> Int -> [[a]]
minor [] _ _ = error "Error input Matrix"
minor m row col = deleteRow m1 row where m1 = (deleteColumn m col)

det :: (Num a, Fractional a) => [[a]] -> a
det [] = error "Error input Matrix"
det [[a]] = a
det m = sum [a*s*(det m1) | i <- [0..dim-1], let a = (head m !! i), let m1 = (minor m i 0), let s = (-1)^i] 
	where
		dim = length m
		
		
--matrice inversa

invert :: (Num a, Fractional a) => [[a]] -> [[a]]
invert [[]] = [[]]
invert m = transpose [[s*(det m1)/d | i <- [0..dim-1], let m1 = (minor m i j), let s = (-1)^(i+j)] | j <- [0..dim-1]]
		where
			dim = length m
			d = det m
		
		