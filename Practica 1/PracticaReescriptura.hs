type Signature = [(String, Integer)]
type Position = [Int]
data Substitution a = Substitution [(a, a)]

class Rewrite a where

	-- rep un objecte de tipus a i retorna una llista d’objectes de tipus a
	getVars :: a -> [a]

	-- rep una Signature i un objecte de tipus a i retorna un Bool,
	valid :: Signature -> a -> Bool

	-- rep dos objectes de tipus a i retorna una llista de parells de
	-- position i subtitution del tipus a
	match :: a -> a -> [(Position, Substitution a)]-- [([Int], [(a,a)])]

	-- rep un objecte del tipus a, i una substitucio d’objectes de
	-- tipus a, i retorna el resultat que sera un objecte del tipus a
	apply :: a -> Substitution a -> a

	-- rep un objecte del tipus a i una llista de parells de position,
	-- un altre objecte del tipus a i retorna el resultat, que sera del tipus a
	replace :: a -> [(Position, Position)] -> a -> a

	-- rep un objecte de tipus a i retorna un objecte de tipus a
	evaluate :: a -> a

data Rule ru = Rule ru ru

data RewriteSystem rs = RewriteSystem [Rule rs]

data RString = RString String

instance Show ru => Show (Rule ru) where
	show (Rule a b) = (show a) ++ " --> " ++ (show b)

instance Show rs => Show (RewriteSystem rs) where
	show (RewriteSystem []) = []
	show (RewriteSystem l) = showRegles l

instance Show s => Show (Substitution s) where
	show (Substitution []) = []
	show (Substitution l) = showSubstitution l

showSubstitution :: Show s => [s] -> [Char]
showSubstitution [] = ""
showSubstitution (x:xs) = (show x) ++ "\n" ++ showSubstitution xs

showRegles :: Show rs => [rs] -> [Char]
showRegles [] = ""
showRegles (x:xs) = (show x) ++ "\n" ++ showRegles xs


validRule :: (Rewrite a) => Signature -> (Rule a) -> Bool
validRule s (Rule a b) = (valid s a) && (valid s b)

validRewriteSystem :: Rewrite a => Signature -> RewriteSystem a -> Bool
validRewriteSystem s (RewriteSystem rs) = and (map (validRule s) rs)

myHead :: RString -> [Char]
myHead (RString s) = [head (s)]

rStringToChar :: RString -> [Char]
rStringToChar (RString s) = s

myTail :: RString -> RString
myTail (RString s) = RString (tail (s))


myLength :: RString -> Int
myLength (RString s) = length (s)

myTake :: Int -> RString -> RString
myTake n (RString s) = RString (take n (s))

myDrop :: Int -> RString -> RString
myDrop n (RString s) = RString (drop n (s))

myEqual :: RString -> RString -> Bool
myEqual (RString s1) (RString s2)
	| s1 == s2 = True
	| otherwise = False

instance Rewrite RString where
	getVars s = []

	valid [] _ = False
	valid (sig:[]) s
		| ((myHead s) == fst sig) && (snd sig == 0) = True
		| otherwise = False
	valid (sig:sigs) s
		| ((myHead s) == fst sig) && (snd sig == 0) = True
		| otherwise = valid sigs s

	match s1 s2 = matching s1 s2 (myLength s2) 0 (myLength s1)

	apply s1 (Substitution []) = s1
	apply s1 (Substitution (par:[])) =  applyAux s1 (snd par) (matchToList (match s1 (fst par)))
	apply s1 (Substitution (par:pars)) = apply (applyAux s1 (snd par) (matchToList (match s1 (fst par)))) (Substitution pars)

lRStringToRString :: [RString] -> RString
lRStringToRString (s:[]) = (s)

lRStringTolChar :: (RString, RString) -> [Char]
lRStringTolChar s = (rStringToChar (fst s)) ++ (rStringToChar (snd s))

applyFinal :: RString -> [RString] -> [Char]
applyFinal s3 (ss)
	| (myLength s3) == 1 = lRStringTolChar (s3, (lRStringToRString ss))
	| otherwise = lRStringTolChar (s3, (myDrop (myLength s3) (lRStringToRString ss)))

applySub :: (Eq a, Num a) => RString -> RString -> a -> a -> [Char]
applySub (RString (s1:ss)) s3 list ite
	| list == ite = applyFinal s3 [(RString ss)]
	| otherwise = [s1] ++ (applySub (RString ss) s3 list (ite + 1))

lCharToRString :: (Eq a, Num a) => RString -> RString -> a -> RString
lCharToRString s1 s3 list = (RString crida)
	where crida = applySub s1 s3 list 0

applyAux :: (Eq a, Num a) => RString -> RString -> [a] -> RString
applyAux s1 _ [] = s1
applyAux s1 s3 (list:[]) = lCharToRString s1 s3 (list)
applyAux s1 s3 (list:lists) = applyAux (lCharToRString s1 s3 list) (s3) lists

matchToList :: [(Position, Substitution a)] -> [Int]
matchToList [] =[]
matchToList (eleme:[]) = fst eleme
matchToList (eleme:elemens) = (fst eleme) ++ matchToList elemens

matching :: (Eq t, Num t) => RString -> RString -> Int -> t -> t -> [([t], Substitution a)]
matching s1 s2 n pos ns
	| pos == ns =  []
	| myEqual (myTake n s1) s2 = [([pos], (Substitution []))] ++ (matching (myTail s1) s2 n (pos+1) ns)
	| otherwise = matching (myTail s1) s2 n (pos+1) ns


readRString :: String -> RString
readRString s = RString s

readRStringSystemAux :: [(String, String)] -> [Rule RString]
readRStringSystemAux [] = []
readRStringSystemAux (x:[]) = [convert]
	where convert = Rule (readRString (fst x)) (readRString (snd x))
readRStringSystemAux (x:xs) = [convert] ++ (readRStringSystemAux xs)
	where convert = Rule (readRString (fst x)) (readRString (snd x))

readRStringSystem :: [(String, String)] -> RewriteSystem RString
readRStringSystem [] = RewriteSystem []
readRStringSystem l = RewriteSystem (readRStringSystemAux l)

instance Show RString where
    show (RString s) = init (tail (show s))