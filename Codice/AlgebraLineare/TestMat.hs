import Matrix
import Utility

bigMat = makeMat 20
smallMat = makeMat 8
iden = identity 20

f1 = prodMatrix bigMat bigMat
f2 = prodMatPar bigMat bigMat

f3 = powMatrix bigMat 2
f4 = powMatPar bigMat 2

f5 = sumMatrix bigMat bigMat
f6 = sumMatPar bigMat bigMat

f7 = det iden
f8 = pardet iden

f9 = det2 iden
f10 = pardet2 iden

f11 = invert smallMat

main = do
		t1 <- evalTimed f1
		t2 <- evalTimed f2
		
		t3 <- evalTimed f3
		t4 <- evalTimed f4
		
		t5 <- evalTimed f5
		t6 <- evalTimed f6
		
		t7 <- evalTimed f7
		t8 <- evalTimed f8
		
		t9 <- evalTimed f9
		t10 <- evalTimed f10
		
		t11 <- evalTimed f11
		
		putStrLn ("Prod seq time = " ++ show t1)
		putStrLn ("Prod par time = " ++ show t2)
		
		putStrLn ("Pow seq time = " ++ show t3)
		putStrLn ("Pow par time = " ++ show t4)
		
		putStrLn ("Sum seq time = " ++ show t5)
		putStrLn ("Sum par time = " ++ show t6)
		
		putStrLn ("Inefficient Det seq time = " ++ show t7)
		putStrLn ("Inefficient Det par time = " ++ show t8)
		
		putStrLn ("Efficient Det seq time = " ++ show t9)
		putStrLn ("Efficient Det par time = " ++ show t10)
		
		putStrLn ("Invert seq time = " ++ show t11)
		
		