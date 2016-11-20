module Utility where


import Control.Parallel
import Control.Parallel.Strategies
import Control.Exception
import Data.Time.Clock
import Text.Printf
import System.Environment
import System.Random

randomList :: Int -> [Int]
randomList n = take n (randomRs (1, n) (mkStdGen 42))

evalTimed f = do 
                 start <- getCurrentTime
                 evaluate f
                 end <- getCurrentTime
                 return (diffUTCTime end start)
				 
				 
printTimeSince t0 = do
  t1 <- getCurrentTime
  printf "time: %.2fs\n" (realToFrac (diffUTCTime t1 t0) :: Double)