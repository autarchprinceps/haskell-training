mapcomb func l m = [func a b | a <- l, b <- m]

mapsingle _ _ [] = []
mapsingle func x (l:ls) = func x l : mapsingle func x ls
