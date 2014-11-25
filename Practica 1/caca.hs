-- String -> Identificador per construir objectes
-- Int -> Nombre de arguments que reben
type Signature = [(String,Int)]

-- Int -> Posicio dins de lobjecte
type Position = [Int]

-- Primer element -> una variable
-- Segon element -> terme pel cual substituim
type Substitution a = [(a,a)]

class Rewrite a where
    getVars :: a -> [a] -- [a] son les variables que apareixen a l'objecte rebut.
    valid :: Signature -> a -> Bool --Indica si l'objecte esta construit correctament seguint la signatura
    match :: a -> a -> [(Position,Substitution a)] -- indica en quines posicions del segon objecte el primer fa matching amb la corresponent substitucio. Es a dir, que aplicant la substitucio al primer tenim exactament els subobjectes que esta a la posicio
    apply :: a -> Substitution a -> a -- retorna el objecte resultat de substituir les variables pels objectes de la substitucio
    replace :: a -> [(Position,a)] -> a -- retorna el resultat de canviar cada subobjecte del primer parametre en una de les posicions donades per l'objecte que l'acompanya. No hi ha solapament.
    evaluate :: a -> a -- retorna el resultat d'haver avaluat l'objecte segons alguns simbols predefinits, que no es tracten amb regles de reescritura


data Rule a = Rule a a

type RewriteSystem a = [Rule a]

instance (Show a) => Show (Rule a) where
    show(Rule a b)= show a ++ " -> " ++ show b


validRule :: Rewrite a => Signature -> Rule a -> Bool
validRule s (Rule a b) = (valid s a) && (valid s b)

validRewriteSystem :: Rewrite a => Signature -> RewriteSystem a -> Bool
validRewriteSystem s ws = and $ map (validRule s) ws

type Estrategia a = [(Position,a)] -> [(Position,a)]

oneStepRewrite :: Rewrite a => RewriteSystem a -> a -> Estrategia a -> a
oneStepRewrite rules obj est = applyObjSubstitution obj $ matchObjRewriteS rules obj
    where
        matchObjRewriteS :: RewriteSystem a -> a -> [(Position,Substitution a)]
        matchObjRewriteS rules obj = concat $ map (\(Rule a _) -> match obj a) rules
        applyObjSubstitution :: a -> [(Position, Substitution a)] -> [a]
        applyObjSubstitution obj l = map (apply obj) [ snd s | s <- l]
--        filtraPorStrat :: Estrategia a -> [a] -> [a]

--buscar de la paerte izquierda de la regla, el match con el objeto y devuelve possubstitucion. Filtrar por la estrategia, que se le pasa la pos, y obj original.

auxrewrite :: Rewrite a => RewriteSystem a -> a -> Estrategia a -> [a]
auxrewrite rules obj est = iterate (\x -> evaluate(oneStepRewrite rules x est)) obj

rewrite :: (Rewrite a, Eq a) => RewriteSystem a -> a -> Estrategia a -> a
rewrite rules obj est = fst $ head $ dropWhile (\x -> fst x /= snd x) $ zip todos (drop 1 $ todos)
    where todos = auxrewrite rules obj est

nrewrite :: Rewrite a => RewriteSystem a -> a -> Estrategia a -> Int -> a
nrewrite rules obj est n = last $ take n $ auxrewrite rules obj est


data RString = RString String

--instance Rewrite RString where
--    getVars s = []

--    valid [] _ = True
--    valid sig@((s,i):xs) (RString st) = and (map (\x -> ) sig)-- No se que cojones es una signatura y como se spune que voy a saber si algo es valid

--    match (RString s1) (RString s2) = sMatch s1 s2
--        where
--            sMatch :: String -> String -> [(Position,Substitution a)]
--            sMatch (c1:s1) (c2:s2) =

--    evaluate s = s

readRString :: String -> RString
readRString s = RString s

readRStringSystem :: [(String,String)] -> RewriteSystem RString
readRStringSystem l = map (\x -> (Rule (readRString (fst x)) (readRString (snd x)))) l

instance Show (RString) where
    show(RString s)= tail $ init $ (show s)

data RTerm a = RTerm a [Rterm a]

--las de mas abajo :> todas las que su camino no es prefijo de otro

--takeWhile dropWhile

--[entran]-> (arbre,[sobran])
