module Graph where

import Data.List

type Graph = (Int, [Edge])
type Edge = ((Int, Int), Int)
type AdjMat = [[Bool]]

--matrice di adiacenza vuota

emptyAdjMat :: Int -> AdjMat
emptyAdjMat dim = emptyAdjMat' dim 0

emptyAdjMat' :: Int -> Int -> AdjMat
emptyAdjMat' n m
			| m >= n = []
			|otherwise = [i == m | i <- [0..n-1]]:(emptyAdjMat' n (m+1))

			
--matrice di adiacenza di un grafo			

makeAdjMat :: Graph -> AdjMat
makeAdjMat (nodes, []) = emptyAdjMat nodes
makeAdjMat (nodes, ((x, y), _):edges) = setEdge (x, y) True (makeAdjMat (nodes, edges))

setEdge :: (Int, Int) -> Bool -> AdjMat -> AdjMat
setEdge _ _ [] = error "Error in setEdge"
setEdge (x, y) v m = replacePosition x y v (replacePosition y x v m)

replacePosition :: Int -> Int -> a -> [[a]] -> [[a]]							
replacePosition x y v m = left ++ [new] ++ right
							where
								(left, old:right) = splitAt x m
								(left2, _:right2) = splitAt y old
								new = left2 ++ [v] ++ right2

--"prodotto" di matrici di adiacenza
						
prodAdjMat :: AdjMat -> AdjMat -> AdjMat
prodAdjMat m1 m2 = [[orVect a b | b <- column] | a <- m1]
						where column = transpose m2
						
								
orVect :: [Bool] -> [Bool] -> Bool
orVect a b = foldOr (zipWith (&&) a b)

foldOr :: [Bool] -> Bool
foldOr [x] = x
foldOr (x:xs) = x || (foldOr xs)

				
--sviluppo n-esimo delle matrici di adiacenza (n scatti)
						
powAdjMat :: AdjMat -> Int -> AdjMat
powAdjMat m 0 = emptyAdjMat dim where dim = length m
powAdjMat m 1 = m
powAdjMat m n = prodAdjMat m (powAdjMat m (n-1))
