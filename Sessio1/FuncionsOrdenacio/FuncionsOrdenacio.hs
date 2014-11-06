insert :: [Int] -> Int -> [Int]
insert [] y = [y]
insert [x] y
	| x <= y = [x, y]
	| otherwise = [y, x]
insert (x:(xs:xss)) y
	| x <= y && xs > y = [x] ++ [y] ++ (xs:xss)
	| x > y = [y] ++ (x:(xs:xss))
	| otherwise = [x] ++ insert (xs:xss) y

insertAll :: [Int] -> [Int] -> [Int]
insertAll [] l = l
insertAll (x:xs) l = insertAll xs (insert l x)

isort :: [Int] -> [Int]
isort [] = []
isort l = insertAll l []

remove :: [Int] -> Int -> [Int]
remove [] y = []
remove (x:[]) y = []
remove (x:xs) y
	| x == y = xs
	| otherwise = [x] ++ remove xs y

ssort :: [Int] -> [Int]
ssort [] = []
ssort [x] = [x]
ssort l = [minElem] ++ ssort (remove l minElem)
	where minElem = minimum l

merge :: [Int] -> [Int] -> [Int]
merge [] [] = []
merge [] l2 = l2
merge l1 [] = l1
merge (x:xs) (y:ys)
	| x < y = [x] ++ merge xs (y:ys)
	| otherwise = [y] ++ merge (x:xs) ys

primeraMeitat :: [Int] -> Int -> [Int]
primeraMeitat l n = take a l
	where a = quot n 2

segonaMeitat :: [Int] -> Int -> [Int]
segonaMeitat l n = drop a l
	where a = quot n 2

msort :: [Int] -> [Int]
msort [] = []
msort [x] = [x]
msort (x:(xs:[])) = merge [x] [xs]
msort l = merge (msort (primeraMeitat l (length l))) (msort (segonaMeitat l (length l)))

majorsList :: Ord a => [a] -> a -> [a]
majorsList [] _ = []
majorsList (x:xs) n
	| x > n = (x:majorsList xs n)
	| otherwise = majorsList xs n

menorsList :: Ord a => [a] -> a -> [a]
menorsList [] _ = []
menorsList (x:xs) n
	| x <= n = (x: menorsList xs n)
	| otherwise = menorsList xs n

qsort :: [Int] -> [Int]
qsort [] = []
qsort [x] = [x]
qsort (x:xs) = qsort (menorsList xs x) ++ [x] ++ qsort (majorsList xs x)