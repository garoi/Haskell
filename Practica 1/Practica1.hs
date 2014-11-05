class Point p where

    --donat un número que indica la coordenada i un element de tipus p
    --retorna el valor de la coordenada que és un Double
    sel :: Int -> p -> Double

    -- donat un element de tipus p retorna la seva dimensió
    dim :: p -> Int

    --donats dos elements e1 i e2 de tipus p que representen
    --punts i una llista de coordenades seleccionades retorna el número del
    --fill de e2 que li toca a e1
    child :: p -> p -> [Int] -> Int

    --donats dos elements e1 i e2 de tipus p ens retorna un Double
    --que  ́és la distància entre e1 i e2
    dist :: p -> p -> Double

    --rep una llista de Double i retorna un element de tipus p
    listToPoint :: [Double] -> p

    ptrans:: [Double] -> p -> p
    pscale :: Double -> p -> p

data Point3d = Point3d [Double] deriving (Eq)

instance Point Point3d where

    sel cxyz (Point3d llc) = llc !! cxyz

    dim (Point3d p) = 3

    child e1 e2 llc = passarBinariADec (reverse ll2) 1
        where ll2 = map (compara e1 e2) llc

    dist (Point3d[x1, y1, z1]) (Point3d[x2, y2, z2]) = sqrt(((x2 - x1) ** 2) + ((y2 - y1) ** 2) + ((z2 - z1) ** 2))

    listToPoint ll = (Point3d ll)
    ptrans [x1, y1, z1] (Point3d [x2, y2, z2]) = (Point3d [(x1+x2), (y1+y2), (z1+z2)])
    pscale fac (Point3d[x, y, z]) = (Point3d [(x*fac), (y*fac), (z*fac)])

instance Show Point3d where
    show (Point3d[x,y,z]) = "(" ++ (show x) ++ ", " ++ (show y) ++ ", " ++ (show z) ++ ")"


data Kd2nTree p = EmptyTree | Node p [Int] [Kd2nTree p]


instance (Point p, Eq p) => Eq (Kd2nTree p) where
    EmptyTree == EmptyTree = True
    a1 == a2 = and [(compararPunts lla1 lla2 ), (length lla1) == (length lla2)]
        where lla1 = get_all a1
              lla2 = get_all a2



instance Show p => Show (Kd2nTree p) where
  show EmptyTree = ""
  show (Node punt coord fills) = (show punt) ++ " " ++ (show coord) ++ "\n" ++ showFills fills 0 0

--Funcions auxiliars
compara :: (Point p) => p -> p -> Int -> Int
compara e1 e2 cxyz
    | (sel (cxyz - 1) e2) <= (sel (cxyz - 1) e1) = 0
    | otherwise = 1

passarBinariADec :: [Int] -> Int -> Int
passarBinariADec [ll2] t = ll2*t
passarBinariADec (ll2:lls2) t = ll2*t + passarBinariADec lls2 (t*2)

showFills :: Show p => [Kd2nTree p] -> Int -> Int -> String
showFills [] num tab = ""
showFills (EmptyTree : cua) num tab = (showFills cua (num + 1) tab)
showFills ((Node p coords fills) : cua) num tab = (take tab (cycle "\t")) ++ "<" ++ (show num) ++ "> " ++ (show p) ++
                                                        " " ++ (show coords) ++ "\n" ++
                                                        showFills fills 0 (tab + 1) ++
                                                        showFills cua (num + 1) tab

compararPunts:: (Point p, Eq p) => [(p, [Int])]->[(p, [Int])] -> Bool
compararPunts [][] = True
compararPunts ((pl1, _) : xs) ((pl2, _) : ys)
  |pl1 == pl2    = compararPunts xs ys
  |otherwise   = False
nouFills :: (Point p) => Int -> Kd2nTree p-> [Kd2nTree p] -> [Kd2nTree p]
nouFills 0 punts (x : xs) = punts : xs
nouFills n punts (x : xs) = x : nouFills (n - 1) punts xs

build :: (Eq p, Point p) => [(p, [Int])] -> Kd2nTree p
build [] = EmptyTree
build [(punts, llc)] = Node punts llc (take (2 ^ (length llc)) (cycle [EmptyTree]))
build (cap:cua) = passarNode (build [cap]) cua

passarNode :: Point p => Kd2nTree p -> [(p, [Int])] -> Kd2nTree p
passarNode punts [] = punts
passarNode punts ((punt2, llc2):ficua) = passarNode (insert punts punt2 llc2) ficua


insert :: Point p => Kd2nTree p -> p -> [Int] -> Kd2nTree p
insert EmptyTree punts llcord = Node punts llcord (take (2 ^ (length llcord)) (cycle [EmptyTree]))
insert (Node punt1 coord fills) punt2 llcoord = Node punt1 coord (nouFills (child punt1 punt2 coord) (insert (head (drop (child punt1 punt2 coord) fills)) punt2 llcoord) fills)

ajuntar [] = []
ajuntar (x:xs) = concat [x, ajuntar xs]

get_all :: Point p => Kd2nTree p -> [(p, [Int])]
get_all EmptyTree = []
get_all (Node punt coord fill) = (punt, coord) : (ajuntar (map get_all fill))

busca_llista [] point = False
busca_llista ((x,_):xs) point
    | x == point = True
    | otherwise = busca_llista xs point

contains :: (Point p, Eq p) => Kd2nTree p -> p -> Bool
contains EmptyTree point = False
contains k point = busca_llista (get_all k) point


elimina_punt :: (Point p, Eq p) => [(p,[Int])] -> p -> [(p,[Int])]
elimina_punt [] point = []
elimina_punt ((x,llista):xs) point
    | x /= point = (x, llista) : elimina_punt xs point
    | otherwise = xs

remove :: (Point p, Eq p) => Kd2nTree p -> p -> Kd2nTree p
remove EmptyTree point = EmptyTree
remove k point = build (elimina_punt (get_all k) point)

busca_punts :: (Point p, Eq p) => [(p,[Int])] -> p -> p -> p
busca_punts [] point puntomejor = puntomejor
busca_punts ((x,_):xs) point puntomejor
    | x == point = busca_punts [] x x
    | (dist x point) < (dist point puntomejor) = busca_punts xs point x
    | otherwise = busca_punts xs point puntomejor

nearest :: (Point p, Eq p) => Kd2nTree p -> p -> p
nearest t@(Node p c f) point = busca_punts (get_all t) point p

kdmap :: (Point p, Point q) => (p -> q)  -> Kd2nTree p -> Kd2nTree q
kdmap _ EmptyTree = EmptyTree
kdmap func (Node p c f) = (Node (func p) c f')
    where f' = map (kdmap func) f

translation :: (Point p) => [Double] -> Kd2nTree p -> Kd2nTree p
translation fact arbre = kdmap (ptrans fact) arbre

scale :: (Point p) => Double -> Kd2nTree p -> Kd2nTree p
scale fact arbre = kdmap (pscale fact) arbre

buildIni :: (Point p, Eq p) => [([Double], [Int])] -> Kd2nTree p
buildIni [] = EmptyTree
buildIni list = build (llistaPoints list)
    where llistaPoints::(Point p , Eq p) => [([Double], [Int])] -> [(p, [Int])]
          llistaPoints [(punt, llista)] = [(listToPoint punt, llista)]
          llistaPoints ((punt, llista):puntsArbre) = [(listToPoint punt, llista)] ++ llistaPoints puntsArbre

exempleSet :: Kd2nTree Point3d
exempleSet =  build [(Point3d [3.0, -1.0, 2.1], [1, 3]),
                    (Point3d [3.5, 2.8, 3.1], [1, 2]),
                    (Point3d [3.5, 0.0, 2.1], [3]),
                    (Point3d [3.0, -1.7, 3.1], [1, 2, 3]),
                    (Point3d [3.0, 5.1, 0.0], [2]),
                    (Point3d [1.5, 8.0, 1.5], [1]),
                    (Point3d [3.3, 2.8, 2.5], [3]),
                    (Point3d [4.0, 5.1, 3.8], [2]),
                    (Point3d [3.1, 3.8, 4.8], [1, 3]),
                    (Point3d [1.8, 1.1, -2.0], [1, 2])
                    ]