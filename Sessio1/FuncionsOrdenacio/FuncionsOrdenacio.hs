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