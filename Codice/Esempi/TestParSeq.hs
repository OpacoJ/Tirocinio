import Control.Parallel
import Control.Parallel.Strategies
import Control.Exception
import Data.Time.Clock
import Text.Printf
import System.Environment
import Esempi
import Utility


main = do
  [n] <- getArgs
  let test = [test1,test2,test3] !! (read n - 1)
  t0 <- getCurrentTime
  r <- evaluate (runEval test)
  printTimeSince t0
  print r
  printTimeSince t0


test1 = return parOp

test2 = return parOp2

test3 = return parOp3
  