mapcomb func l m [] [] = [func a b | a <- l, b <- m]
mapcomb func l m p1 p2 = [func a b p1 p2 | a <- l, b <- m]

mapsingle _ _ [] _ _ = []
mapsingle func x (l:ls) [] [] = func x l : mapsingle func x ls [] []
mapsingle func x (l:ls) p1 p2 = func x l p1 p2 : mapsingle func x ls p1 p2

keyvaluetester key l
	| fst l == key = snd l
	| otherwise = []

keyvalue [] _ = []
keyvalue (l:ls) x = if fst l == x then snd l else keyvalue ls x
