myLength :: [Int] -> Int
myLength [] = 0
myLength [n] = 1
myLength (x:xs) = 1 + myLength (xs)

myMaximum :: [Int] -> Int
myMaximum [n] = n
myMaximum (x:xs) = max x (myMaximum xs)

sumList :: [Int] -> Int
sumList [] = 0
sumList [n] = n
sumList (x:xs) = x + sumList xs

average :: [Int] -> Float
average [n] = fromIntegral n
average l = fromIntegral (sumList l) / fromIntegral (myLength l)

reverseList :: [Int] -> [Int]
reverseList [] = []
reverseList [n] = [n]
reverseList (x:xs) = reverseList xs ++ [x]


buildPalindrome :: [Int] -> [Int]
buildPalindrome [] = []
buildPalindrome l = reverseList l ++ l

deleteNumber :: [Int] -> Int -> [Int]
deleteNumber [] _ = []
deleteNumber (x:xs) y
	| x == y = deleteNumber xs y
	| otherwise = [x] ++ (deleteNumber xs y)

remove :: [Int] -> [Int] -> [Int]
remove x [] = x
remove [] _ = []
remove x (y:ys) = remove (deleteNumber x y) ys

flatten :: [[Int]] -> [Int]
flatten [] = []
flatten (x:xs) = x ++ flatten xs


parells :: [Int] -> [Int]
parells [] = []
parells (x:xs)
	| mod x 2 == 0 = [x] ++ parells xs
	| otherwise = parells xs

senars :: [Int] -> [Int]
senars [] = []
senars (x:xs)
	| mod x 2 /= 0 = [x] ++ senars xs
	| otherwise = senars xs

oddsNevens :: [Int] -> ([Int], [Int])
oddsNevens [] = ([],[])
oddsNevens l = (senars l, parells l)

divisio :: Int -> Int -> Bool
divisio n d
	| d == 1 = False
	| mod n d == 0 = True
	| otherwise = divisio n (d - 1)

isPrime :: Int -> Bool
isPrime 0 = False
isPrime 1 = False
isPrime n = not (divisio n (n - 1))

mirarPrimers :: Int -> Int -> [Int]
mirarPrimers x 1 = []
mirarPrimers x y
	| (mod x y) == 0 && isPrime y = mirarPrimers x (y - 1) ++ [y]
	| otherwise = mirarPrimers x (y - 1)

primeDivisors :: Int -> [Int]
primeDivisors 1 = []
primeDivisors x
	| isPrime x = [x]
	| otherwise = mirarPrimers x (x - 1)