import Algebra_Lineare
import Utility

bigMat = makeMat 600 :: [[Int]]

f1 = prodMatrix bigMat bigMat
f2 = prodMatPar bigMat bigMat
f3 = powMatrix bigMat 2
f4 = powMatPar bigMat 2
f5 = sumMatrix bigMat bigMat
f6 = sumMatPar bigMat bigMat

main = do
		t1 <- evalTimed f1
		t2 <- evalTimed f2
		t3 <- evalTimed f3
		t4 <- evalTimed f4
		t5 <- evalTimed f5
		t6 <- evalTimed f6
		putStrLn ("Prod seq time = " ++ show t1)
		putStrLn ("Prod par time = " ++ show t2)
		putStrLn ("Pow seq time = " ++ show t3)
		putStrLn ("Pow par time = " ++ show t4)
		putStrLn ("Sum seq time = " ++ show t5)
		putStrLn ("Sum par time = " ++ show t6)