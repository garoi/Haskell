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
	match :: a -> a -> [(Position, Substitution a)]

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

leftmost obj (l:ls) a b = applyAux obj a b [l]

rightmost obj l a b = applyAux obj a b [(l !! ((length l) - 1))]

oneStepRewrite :: (Rewrite a) => RewriteSystem a -> a -> (a -> [Int] -> a -> a -> a) -> a
oneStepRewrite (RewriteSystem []) obj f = obj
oneStepRewrite (RewriteSystem ((Rule a b):sistemes)) obj f
	| matchToList (match obj a) /= [] = f obj (matchToList (match obj a)) a b
	| otherwise = oneStepRewrite (RewriteSystem sistemes) obj f


rewrite :: (Rewrite a) => RewriteSystem a -> a -> (a -> [Int] -> a -> a -> a) -> a
rewrite (RewriteSystem s@((Rule a b):sistemes)) obj f
	| matchToList (match obj a) /= [] = rewrite (RewriteSystem s) (evaluate (oneStepRewrite (RewriteSystem s) obj f)) f
	| otherwise = final (RewriteSystem s) obj f

final :: Rewrite a => RewriteSystem a -> a -> (a -> [Int] -> a -> a -> a) -> a
final (RewriteSystem s@((Rule a b):sistemes)) obj f
	| snd (rewriteAux (RewriteSystem sistemes) obj f) == True = obj
	| otherwise = rewrite (RewriteSystem s) (fst (rewriteAux (RewriteSystem s) obj f)) f

rewriteAux :: Rewrite a => RewriteSystem a -> a -> (a -> [Int] -> a -> a -> a) -> (a, Bool)
rewriteAux (RewriteSystem []) obj f = (obj, True)
rewriteAux (RewriteSystem s@((Rule a b):[])) obj f
	| matchToList (match obj a) /= [] = ((evaluate (oneStepRewrite (RewriteSystem s) obj f)), False)
	| otherwise = (obj, True)
rewriteAux (RewriteSystem s@((Rule a b):sistemes)) obj f
	| matchToList (match obj a) /= [] = ((evaluate (oneStepRewrite (RewriteSystem s) obj f)), False)
	| otherwise = rewriteAux (RewriteSystem sistemes) obj f

nrewrite :: (Rewrite a) => RewriteSystem a -> a -> (a -> [Int] -> a -> a -> a) -> Int -> a
nrewrite (RewriteSystem s) obj f ite = nrewriteAux (RewriteSystem s) obj f ite 1

nrewriteAux (RewriteSystem s) obj f ite ite2
	| ite == ite2 = evaluate (oneStepRewrite (RewriteSystem s) obj f)
	| otherwise = nrewriteAux (RewriteSystem s) (evaluate (oneStepRewrite (RewriteSystem s) obj f)) f (ite) (ite2 + 1)


instance Rewrite RString where
	getVars s = []

	valid [] _ = False
	valid sig s
		| myEqual (myTail s) (RString "")  = and (comprovar sig (myHead s))
		| otherwise = and (comprovar sig (myHead s) ++ [valid sig (myTail s)])

	match s1 s2 = matching s1 s2 (myLength s2) 0 (myLength s1)

	apply s1 (Substitution []) = s1
	apply s1 (Substitution (par:[])) =  applyAux s1 (fst par) (snd par) (matchToList (match s1 (fst par)))
	apply s1 (Substitution (par:pars)) = apply (applyAux s1 (fst par) (snd par) (matchToList (match s1 (fst par)))) (Substitution pars)

	replace s1 [] _ = s1
	replace s1 (l:[]) s2 = replaceAux s1 l s2
	replace s1 (l:ls) s2 = replace (replaceAux s1 l s2) ls s2

	evaluate s1 = s1

comprovarAux :: String -> [Char] -> Bool
comprovarAux [] _ = False
comprovarAux (s:[]) a
	| s == a !! 0 = True
	| otherwise = False
comprovarAux (s:ss) a
	| s == (a !! 0) = True
	|otherwise = comprovarAux ss a

comprovar :: Signature -> [Char] -> [Bool]
comprovar [] _ = []
comprovar (sig:[]) a
	| (comprovarAux (fst sig) a) && (snd sig == 0) = [True]
	| otherwise = [False]
comprovar (sig:sigs) a
	| (comprovarAux (fst sig) a) && (snd sig == 0) = [True]
	| otherwise = comprovar sigs a

lRStringToRString :: [RString] -> RString
lRStringToRString (s:[]) = (s)

lRStringTolChar :: (RString, RString) -> [Char]
lRStringTolChar s = (rStringToChar (fst s)) ++ (rStringToChar (snd s))

applyFinal :: RString -> RString -> [RString] -> [Char]
applyFinal s3 s2 (ss)
	| (myLength s2) == 1 = lRStringTolChar (s3, (lRStringToRString ss))
	| otherwise = lRStringTolChar (s3, (myDrop ((myLength s2)-1) (lRStringToRString ss)))

applySub :: (Eq a, Num a) => RString -> RString -> RString -> a -> a -> [Char]
applySub (RString (s1:ss)) s2 s3 list ite
	| list == ite = applyFinal s3 s2 [(RString ss)]
	| otherwise = [s1] ++ (applySub (RString ss) s2 s3 list (ite + 1))

lCharToRString :: (Eq a, Num a) => RString -> RString -> RString -> a -> RString
lCharToRString s1 s2 s3 list = (RString crida)
	where crida = applySub s1 s2 s3 list 0

applyAux :: (Eq a, Num a) => RString -> RString -> RString -> [a] -> RString
applyAux s1 s2 _ [] = s1
applyAux s1 s2 s3 (list:[]) = lCharToRString s1 s2 s3 (list)
applyAux s1 s2 s3 (list:lists) = applyAux (lCharToRString s1 s2 s3 list) s2 (s3) lists

matchToList :: [(Position, Substitution a)] -> [Int]
matchToList [] =[]
matchToList (eleme:[]) = fst eleme
matchToList (eleme:elemens) = (fst eleme) ++ matchToList elemens

matching :: (Eq t, Num t) => RString -> RString -> Int -> t -> t -> [([t], Substitution a)]
matching s1 s2 n pos ns
	| pos == ns =  []
	| myEqual (myTake n s1) s2 = [([pos], (Substitution []))] ++ (matching (myTail s1) s2 n (pos+1) ns)
	| otherwise = matching (myTail s1) s2 n (pos+1) ns


changeFinal :: RString -> [RString] ->[Char]
changeFinal s2 ss = lRStringTolChar (s2, (lRStringToRString ss))

changeAux :: (Eq a, Num a) => RString -> a -> RString -> a -> [Char]
changeAux (RString (s1:ss1)) e1 s2 ite
	| ite == e1 = changeFinal s2 [(RString ss1)]
	| otherwise = [s1] ++ changeAux (RString ss1) e1 s2 (ite + 1)


change :: RString -> Int -> RString -> Int -> RString
change s1 e1 s2 0 = (RString llamada)
	where llamada = changeAux s1 e1 s2 0

takeElem :: RString -> Int -> Int -> RString -> Int -> RString
takeElem s1 e1 e2 s2 ite
	| ite == e2 = change s1 e1 s2 0
	| otherwise = takeElem s1 e1 e2 (myTail s2) (ite + 1)

replaceAuxList :: RString -> Position -> Position -> RString -> RString
replaceAuxList s1 [] _ _ = s1
replaceAuxList s1 _ [] _ = s1
replaceAuxList s1 (l1:[]) (l2:[]) s2 = takeElem s1 l1 l2 s2 0
replaceAuxList s1 (l1:ls1) (l2: ls2) s2 = replaceAuxList (takeElem s1 l1 l2 s2 0) ls1 ls2 s2

replaceAux :: RString -> (Position, Position) -> RString -> RString
replaceAux s1 pos s2 = replaceAuxList s1 (fst pos) (snd pos) s2


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