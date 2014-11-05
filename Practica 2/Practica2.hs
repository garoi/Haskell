ferTaulell :: Int -> Int -> [[Char]]
ferTaulell files columnes = replicate files (replicate columnes " ")

random

main = do
  putStrLn ("Benvingut al joc del SOS")

  putStrLn ("Quantes files vols?")
  files <- getLine

  putStrLn ("I quantes columnes?")
  columnes <- getLine

  putStrLn ("Ara ens has de dir si vols una simulació o jugar contra la CPU")
  putStrLn("1: Simulació")
  putStrLn("2: Contra la CPU")
  mode <- getLine

  ferTaulell (read files::Int) read columnes::Int

  if mode == "1" then
	  do
	  	putStrLn("Go")
	  	random l
	  		where l = ferTaulell files columnes

  else
  	do
  	putStrLn("POI")

  return mode