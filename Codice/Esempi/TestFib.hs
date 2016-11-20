import Control.Parallel
import Control.Parallel.Strategies
import Control.Exception
import Data.Time.Clock
import Text.Printf
import System.Environment
import Utility

fib :: Integer -> Integer
fib 0 = 1
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

main = do
  [n] <- getArgs
  let test = [test1,test2,test3,test4,test5] !! (read n - 1)
  t0 <- getCurrentTime
  r <- evaluate (runEval test)
  printTimeSince t0
  print r
  printTimeSince t0

test1 = return (fib 36,fib 35)

test2 = do
  x <- rpar (fib 36)
  y <- rpar (fib 35)
  return (x,y)

test3 = do
  x <- rpar (fib 36)
  y <- rseq (fib 35)
  return (x,y)

test4 = do
  x <- rpar (fib 36)
  y <- rseq (fib 35)
  rseq x
  return (x,y)

test5 = do
  x <- rpar (fib 36)
  y <- rpar (fib 35)
  rseq x
  rseq y
  return (x,y)

