import Control.Parallel
import Control.Parallel.Strategies
import Control.Exception
import Data.Time.Clock
import Text.Printf
import System.Environment
import Esempi
import Utility

longls = randomList 10000


main = do
  [n] <- getArgs
  let test = [test1,test2,test3,test4] !! (read n - 1)
  t0 <- getCurrentTime
  r <- evaluate (runEval test)
  printTimeSince t0
  print r
  printTimeSince t0

test1 = return (quicksort longls)

test2 = return (parquicksort longls)

test3 =  return (mergesort longls)

test4 =  return (parmergesort longls)
