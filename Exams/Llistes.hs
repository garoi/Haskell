
--[1,2,3,...], la segona [2,3,4,...]
inflists :: [[Integer]]
inflists = [[a..] | a<-[1..]]

takeLESum :: Integer -> [Integer] -> [Integer]
takeLESum _ [] = []
takeLESum s (x:xs)
	| s < x = []
	| otherwise = x:(takeLESum (s-x) xs)

consecutSum :: Integer -> [Integer]
consecutSum n = head (dropWhile (filtre n) (map (takeLESum n) inflists))

filtre n l = sum l /= n