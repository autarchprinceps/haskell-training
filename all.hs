import Data.List

keyvalues l key = map snd (filter check l)
	where check a = fst a == key

powerset [] = [[]]
powerset (l:ls) = rekres ++ map (l:) rekres where rekres = powerset ls

power l = filter (not . null) (powerset l)

allexquantcombinations [] = []
allexquantcombinations (l:[]) = power l
ballexquantcombinations (l:ls) =  mapcomb (\a b -> a:b:[]) (power l) (allexquantcombinations ls)

onion s l = map head ((group . sort) (s ++ foldl (++) [] l))

mapcomb func l m = [func a b | a <- l, b <- m]

mapsingle func x l = map (func x) l

keyvaluetester key l
	| fst l == key = snd l
	| otherwise = []

keyvalue [] _ = []
keyvalue (l:ls) x = if fst l == x then snd l else keyvalue ls x
