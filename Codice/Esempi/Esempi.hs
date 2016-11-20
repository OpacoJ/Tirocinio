module Esempi where

import Control.Parallel
import Control.Parallel.Strategies


fib 0 = 1
fib 1 = 1
fib n = (fib (n-1)) + (fib (n-2))

fibpar n 
	| n < 10 = fib 10
	| otherwise = (n1 `par` n2) `pseq` (n1 + n2)
           where
               n1 = fibpar (n-1)
               n2 = fibpar (n-2)


parMapList :: (a -> b) -> [a] -> [b]
parMapList f ls = map f ls `using` parList rseq


quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = left ++ [x] ++ right where
						left = quicksort [a | a <- xs, a <= x]
						right = quicksort [a | a <- xs, a > x]


parquicksort :: (Ord a) => [a] -> [a]
parquicksort [] = []
parquicksort [x] = [x]
parquicksort (x:xs) = left `par` (right `pseq` (left ++ x:right))
						where
							left = parquicksort [a | a <- xs, a <= x]
							right = parquicksort [a | a <- xs, a > x]


force :: [a] -> ()
force xs = go xs `pseq` ()
			where 
				go (_:xs) = go xs
				go [] = 1


merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys)
	| x <= y = x:(merge xs (y:ys))
	| otherwise = y:(merge (x:xs) ys)
	
forceList :: [a] -> ()
forceList [] = ()
forceList (x:xs) = x `pseq` (forceList xs)

mergesort :: Ord a => [a] -> [a]
mergesort [] = []
mergesort [x] = [x]
mergesort xs = merge (mergesort left) (mergesort right)
				where
					(left, right) = splitAt l xs
					l = div (length xs) 2



parmergesort :: Ord a => [a] -> [a]
parmergesort [] = []
parmergesort [x] = [x]
parmergesort xs = (forceList left) `par` ((forceList right) `pseq` (merge left right))
				where
					(left1, right1) = splitAt l xs
					l = div (length xs) 2
					left = parmergesort left1
					right = parmergesort right1
	
mergesort' [x] = [x]
mergesort' [x,y] = if x < y then [x,y] else [y,x]
mergesort' xs = (forceList sleft) `par`
               (forceList sright) `pseq`
               merge sleft sright
               where
                 (left,right) = splitAt (length xs `div` 2) xs
                 sleft = mergesort' left
                 sright = mergesort' right	

	
	
parOp = f1 `par` (f1 + f2) where
            f1 = fib 35
            f2 = fib 35
			
parOp2 = f1 `par` (f2 + f1) where
            f1 = fib 35
            f2 = fib 35
			
parOp3 = f1 `par` (f2 `pseq` (f1 + f2)) where
            f1 = fib 35
            f2 = fib 35
	
	