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

				  
