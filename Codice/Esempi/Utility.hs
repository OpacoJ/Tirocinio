module Utility where

import Control.Parallel.Strategies
import Control.Exception (evaluate)
import Data.Time.Clock (NominalDiffTime, diffUTCTime, getCurrentTime)


evalTimed f = do 
                 start <- getCurrentTime
                 evaluate (f `using` rdeepseq)
                 end <- getCurrentTime
                 return (diffUTCTime end start)