absValue :: Int -> Int
absValue n
	| n < 0 = n*(-1)
	| otherwise = n

power :: Int -> Int -> Int
power x 0 = 1
power x p  = x * power x (p - 1)

divisio :: Int -> Int -> Bool
divisio n d
	| d == 1 = False
	| mod n d == 0 = True
	| otherwise = divisio n (d - 1)

isPrime :: Int -> Bool
isPrime 0 = False
isPrime 1 = False
isPrime n = not (divisio n (n - 1))

slowFib :: Int -> Int
slowFib n
    | n < 2 = n
    | otherwise = slowFib (n - 1) + slowFib (n - 2)

quickFib :: Int -> Int
quickFib n = fst (fibo n)

fibo :: Int -> (Int, Int)
fibo 0 = (0, 0)
fibo 1 = (1, 0)
fibo n = (a + b, a)
	where (a, b) = fibo (n - 1)