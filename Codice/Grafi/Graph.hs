module Graph where

import Matrix

type Graph = ([Node], [Edge])
type Node = Int
type Edge = (Node, Node)


adjMat :: Graph -> Matrix
adjMat (nodes, []) = zeroes dim
						where
								dim = length nodes
adjMat (nodes, e:edges) = setEdge e 1.0 (adjMat (nodes, edges))

setEdge :: Edge -> Double -> Matrix -> Matrix
setEdge _ _ [] = error "Error out of bound"
setEdge (x, y) v m = replacePosition x y v (replacePosition y x v m)

replacePosition :: Int -> Int -> Double -> Matrix -> Matrix							
replacePosition x y v m = left ++ [new] ++ right
							where
								(left, old:right) = splitAt x m
								(left2, _:right2) = splitAt y old
								new = left2 ++ [v] ++ right2

normalize :: Matrix -> Matrix				
normalize m = [[reduce x | x <- y] | y <- m]
					where reduce a 
							| a==0 = 0
							| otherwise = 1
							
fullyConnected :: Matrix -> Bool
fullyConnected [] = True
fullyConnected (x:xs) = (all (/= 0.0) x) && fullyConnected xs

isConnected :: Matrix -> Bool
isConnected m = checkConnection m1 m1 where m1 = normalize m



checkConnection :: Matrix -> Matrix -> Bool
checkConnection m1 m2
		| fullyConnected m1 = True
		| m3 == m1 = False
		| otherwise = checkConnection m3 m2
			where m3 = normalize (prodMatrix m1 m2)
				

								
								
								