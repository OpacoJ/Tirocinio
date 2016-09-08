import Utility
import Esempi

longls = randomList 5000

f1 = quicksort longls
f2 = parquicksort longls
f3 = mergesort longls
f4 = parmergesort longls


main = do
		t1 <- evalTimed f1
		t2 <- evalTimed f2
		t3 <- evalTimed f3
		t4 <- evalTimed f4
		putStrLn ("Quick seq time = " ++ show t1)
		putStrLn ("Quick par time = " ++ show t2)
		putStrLn ("Merge seq time = " ++ show t3)
		putStrLn ("Merge par time = " ++ show t4)