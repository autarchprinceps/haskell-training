keyvalues l key = map snd (filter check l)
	where check a = fst a == key

powerset [] = [[]]
powerset (l:ls) = rekres ++ map (l:) rekres where rekres = powerset ls

power l = filter (not . null) (powerset l)

allexquantcombinations [] = []
allexquantcombinations (l:[]) = power l
allexquantcombinations (l:ls) = mapcomb (\a b -> [a:b]) (power l) (allexquantcombinations ls) [] []
