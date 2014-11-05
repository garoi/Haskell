module Point where

--classe Point del tipus p
class Point p where

    --donat un número que indica la coordenada i un element de tipus p
    --retorna el valor de la coordenada que és un Double
    sel :: Int -> p -> Double

    -- donat un element de tipus p retorna la seva dimensió
    dim :: p -> Int

    --donats dos elements e1 i e2 de tipus p que representen
    --punts i una llista de coordenades seleccionades retorna el número del
    --fill de e2 que li toca a e1
    child :: p -> p -> [Double] -> Int

    --donats dos elements e1 i e2 de tipus p ens retorna un Double
    --que  ́és la distància entre e1 i e2
    dist :: p -> p -> Double

    --rep una llista de Double i retorna un element de tipus p
    listToPoint :: [Double] -> p
