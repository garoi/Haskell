-- Problema 1.1

inflists :: [[Integer]]
inflists = [ [n..] | n <- [1..]]

-- Problema 1.2

takeLESum :: Integer -> [Integer] -> [Integer]
takeLESum _ [] = []
takeLESum s (x:xs)
	  | s<x = []
	  |otherwise = x:(takeLESum (s-x) xs)

{- una altra solucio:

takeLESum s l = take (length (takeWhile (<=s) (scanl1 (+) l))) l

-}


-- Problema 1.3

consecutSum :: Integer -> [Integer]
consecutSum n = head $ dropWhile (difsum n) $ map (takeLESum n) inflists

difsum :: Integer -> [Integer] -> Bool
difsum n l = (sum l) /= n

-----------------------------------------------

-- Problema 2.1

dc :: (a -> Bool) -> (a -> b) -> (a -> [a]) -> (a -> [b] -> b) -> a -> b

dc trivial solve split combine x
  | trivial x = solve x
  | otherwise = combine x (map (dc trivial solve split combine) (split x))


-- Problema 2.2

merge :: Ord a => [a] -> [[a]] -> [a]
merge _ [l1,l2] = merge' l1 l2

merge' :: Ord a => [a] -> [a] -> [a]
merge' [] l = l
merge' l [] = l
merge' l1@(x:xs) l2@(y:ys)
  | x <= y    = x:(merge' xs l2)
  | otherwise = y:(merge' l1 ys)

divide :: [b] -> [[b]]
divide l = [l1,l2]
  where (l1,l2) = splitAt ((length l) `div` 2) l

mtrivial :: [b] -> Bool
mtrivial [] = True
mtrivial [x] = True
mtrivial _ = False

mergesort :: Ord a => [a] -> [a]
mergesort = dc mtrivial id divide merge

--------------------------------------------

-- Problema 3.1

data Expressio a = Binari (a->a->a) (Expressio a) (Expressio a)
                 | Unari (a->a) (Expressio a)
                 | Fulla a

aval :: (Expressio a) -> a
aval (Binari f a1 a2) =  f (aval a1) (aval a2)
aval (Unari f a1) =  f (aval a1)
aval (Fulla x) = x

instance Eq a => Eq (Expressio a) where
  x == y = aval x == aval y

-- Problema 3.2

data NExpressio a = Nari (a->a->a) [NExpressio a]
                 | NUnari (a->a) (NExpressio a)
                 | NFulla a

naval :: (NExpressio a) -> a
naval (NFulla x) = x
naval (NUnari f a1) =  f (naval a1)
naval (Nari f le) = foldl1 f (map naval le)

instance Eq a => Eq (NExpressio a) where
  x == y = naval x == naval y

--------------------------------------------