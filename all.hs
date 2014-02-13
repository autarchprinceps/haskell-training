import Data.List

keyvalues l key = map snd (filter check l)
	where check a = fst a == key

powerset [] = [[]]
powerset (l:ls) = rekres ++ map (l:) rekres where rekres = powerset ls

power l = filter (not . null) (powerset l)

allexquantcombinations [] = []
allexquantcombinations (l:[]) = power l
allexquantcombinations (l:ls) =  mapcomb (\a b -> a ++ b) (power l) (allexquantcombinations ls)

onion s l = nub (s ++ foldl (++) [] l)

mapcomb func l m = [func a b | a <- l, b <- m]

mapsingle func x l = map (func x) l

keyvaluetester key l
	| fst l == key = snd l
	| otherwise = []

keyvalue [] _ = []
keyvalue (l:ls) x = if fst l == x then snd l else keyvalue ls x

-- Allen.lsp

inverse l = map (keyvalue inverselist) l

combinepair x y
	| x == '=' = [y]
	| y == '=' = [x]
	| otherwise = keyvalue (keyvalue pmatrix x) y

combine l m = nub (mapcomb combinepair l m)

test a g h = foldl (&&) True [testsingle a g h, testsingle (inverse h) a (inverse g), testsingle g (inverse h) (inverse a)]
	where testsingle l m r = intersection (combine l m) r
