type Signature = [(String, Int)]
type Position = [Int]
data Subtitution a = Subtitution [(a, a)]

class Rewrite a where

	-- rep un objecte de tipus a i retorna una llista d’objectes de tipus a
	getVars :: a -> [a]

	-- rep una Signature i un objecte de tipus a i retorna un Bool,
	valid :: Signature -> a -> Bool

	-- rep dos objectes de tipus a i retorna una llista de parells de
	-- position i subtitution del tipus a
	match :: a -> a -> [(Position, Subtitution a)]

	-- rep un objecte del tipus a, i una substitucio d’objectes de
	-- tipus a, i retorna el resultat que sera un objecte del tipus a
	apply :: a -> Subtitution a -> a

	-- rep un objecte del tipus a i una llista de parells de position,
	-- un altre objecte del tipus a i retorna el resultat, que sera del tipus a
	replace :: a -> [(Position, Position)] -> a -> a

	-- rep un objecte de tipus a i retorna un objecte de tipus a
	evaluate :: a -> a

data Rule ru = Rule ru ru | EmptyRule

data RewriteSystem rs = RewriteSystem [Rule rs]

instance Show ru => Show (Rule ru) where
	show EmptyRule = ""
	show (Rule a b) = (show a) ++ " --> " ++ (show b)

instance Show rs => Show (RewriteSystem rs) where
	show (RewriteSystem []) = []
	show (RewriteSystem l) = show l
