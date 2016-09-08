module Utility where

import Control.Parallel.Strategies
import Control.Exception (evaluate)
import Data.Time.Clock (NominalDiffTime, diffUTCTime, getCurrentTime)
import System.Random

randomList :: Int -> [Int]
randomList n = take n (randomRs (1, n) (mkStdGen 42))

evalTimed f = do 
                 start <- getCurrentTime
                 evaluate f
                 end <- getCurrentTime
                 return (diffUTCTime end start)