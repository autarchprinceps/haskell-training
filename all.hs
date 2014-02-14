import Data.List
import qualified Data.Map as M
import qualified Data.Maybe as May
import qualified System.IO as IO

--keyvalues l key = map snd (filter check l)
--	where check a = fst a == key

powerset [] = [[]]
powerset (l:ls) = rekres ++ map (l:) rekres where rekres = powerset ls

power l = filter (not . null) (powerset l)

allexquantcombinations [] = []
allexquantcombinations (l:[]) = power l
allexquantcombinations (l:ls) =  mapcomb (\a b -> a ++ b) (power l) (allexquantcombinations ls)

onion s l = nub (s ++ foldl (++) [] l)

mapcomb func l m = [func a b | a <- l, b <- m]

mapsingle func x l = map (func x) l

-- keyvalues combination 
-- struktur der daten: [('k/g/h',[[»allenrel«],..]),..]
-- hinzufügen zu combination: Map.insertWith (++) »key« [»value«] »map«

testcombination combination lrs = test (onion (keyvalue [] 'k' lrs) (keyvalue [] 'k' combination)) (onion (keyvalue [] 'g' lrs) (keyvalue [] 'g' combination)) (onion (keyvalue [] 'h' lrs) (keyvalue [] 'h' combination))

keyvalue d k l = May.fromMaybe d (M.lookup k l)

--keyvaluetester key l
--	| fst l == key = snd l
--	| otherwise = []

--keyvalue [] _ = []
--keyvalue (l:ls) x = if fst l == x then snd l else keyvalue ls x

-- Allen

inverse l = map (May.fromMaybe "") (map (`M.lookup` inversemap) l)

combinepair x y
	| x == "=" = [y]
	| y == "=" = [x]
	| otherwise = keyvalue [] y (keyvalue (M.empty) x pmatrix)

combine l m = nub (concat (mapcomb combinepair l m))

test a g h = all null [testsingle a g h, testsingle (inverse h) a (inverse g), testsingle g (inverse h) (inverse a)]
	where testsingle l m r = intersect (combine l m) r

-- Main
-- IO ?
parse [] = []
parse ('r':(';':(from:(';':(to:(';':relations))))))
	| null geprsrel = []
	| null pfad = []
	| otherwise = do
		if pfad !! 1 then resultrules = inverse geprsrel else resultrules = geprsrel
		return [False, pfad !! 0, resultrules]
	where geprsrel = words relations
		  pfad = direction from to

readRules name = do
	content <- IO.readFile name
	return [lrs, lE]
	where rules = filter (not . null) (map parse (lines content))

--evaulate file
--	| null lE = test (keyvalue [] 'k' lrs) (keyvalue [] 'g' lrs) (keyvalue [] 'h' lrs)
--	| otherwise = foldl (||) False (map (`testcombination` lrs) (allexquantcombinations lE))
--	where [lrs, lE] = readRules file

-- Static

direction a b
	| a == 'A' && b == 'B' = ['k', False]
	| a == 'B' && b == 'C' = ['g', False]
	| a == 'A' && b == 'C' = ['h', False]
	| a == 'B' && b == 'A' = ['k', True]
	| a == 'C' && b == 'B' = ['g', True]
	| a == 'C' && b == 'A' = ['h', True]
	| otherwise = []

inversemap = M.fromList [("=","="),("<",">"),(">","<"),("d","di"),("di","d"),("o","oi"),("oi","o"),("m","mi"),("mi","m"),("s","si"),("si","s"),("f","fi"),("fi","f")]
pmatrix = M.fromList [("<",M.fromList [("<",["<"]),(">",["=","<",">","d","di","o","oi","m","mi","s","si","f","fi"]),("d",["<","o","m","d","s"]),("di",["<"]),("o",["<"]),("oi",["<","o","m","d","s"]),("m",["<"]),("mi",["<","o","m","d","s"]),("s",["<"]),("si",["<"]),("f",["<","o","m","d","s"]),("fi",["<"])]),(">",M.fromList [("<",["=","<",">","d","di","o","oi","m","mi","s","si","f","fi"]),(">",[">"]),("d",[">","oi","mi","d","f"]),("di",[">"]),("o",[">","oi","mi","d","f"]),("oi",[">"]),("m",[">","oi","mi","d","f"]),("mi",[">"]),("s",[">","oi","mi","d","f"]),("si",[">"]),("f",[">"]),("fi",[">"])]),("d",M.fromList [("<",["<"]),(">",[">"]),("d",["d"]),("di",["=","<",">","d","di","o","oi","m","mi","s","si","f","fi"]),("o",["<","o","m","d","s"]),("oi",[">","oi","mi","d","f"]),("m",["<"]),("mi",[">"]),("s",["d"]),("si",[">","oi","mi","d","f"]),("f",["d"]),("fi",["<","o","m","d","s"])]),("di",M.fromList [("<",["<","o","m","di","fi"]),(">",[">","oi","mi","di","si"]),("d",["=","d","di","o","oi","s","si","f","fi"]),("di",["di"]),("o",["o","di","fi"]),("oi",["oi","di","si"]),("m",["o","di","fi"]),("mi",["oi","di","si"]),("s",["o","di","fi"]),("si",["di"]),("f",["oi","di","si"]),("fi",["di"])]),("o",M.fromList [("<",["<"]),(">",[">","oi","mi","di","si"]),("d",["o","d","s"]),("di",["<","o","m","di","fi"]),("o",["<","o","m"]),("oi",["=","d","di","o","oi","s","si","f","fi"]),("m",["<"]),("mi",["oi","di","si"]),("s",["o"]),("si",["di","fi","o"]),("f",["d","s","o"]),("fi",["<","o","m"])]),("oi",M.fromList [("<",["<","o","m","di","fi"]),(">",[">"]),("d",["oi","d","f"]),("di",[">","oi","mi","di","si"]),("o",["=","d","di","o","oi","s","si","f","fi"]),("oi",[">","oi","mi"]),("m",["o","di","fi"]),("mi",[">"]),("s",["oi","d","f"]),("si",[">","oi","mi"]),("f",["oi"]),("fi",["oi","di","si"])]),("m",M.fromList [("<",["<"]),(">",[">","oi","mi","di","si"]),("d",["o","d","s"]),("di",["<"]),("o",["<"]),("oi",["o","d","s"]),("m",["<"]),("mi",["=","f","fi"]),("s",["m"]),("si",["m"]),("f",["d","s","o"]),("fi",["<"])]),("mi",M.fromList [("<",["<","o","m","di","fi"]),(">",[">"]),("d",["oi","d","f"]),("di",[">"]),("o",["oi","d","f"]),("oi",[">"]),("m",["=","s","si"]),("mi",[">"]),("s",["d","f","oi"]),("si",[">"]),("f",["mi"]),("fi",["mi"])]),("s",M.fromList [("<",["<"]),(">",[">"]),("d",["d"]),("di",["<","o","m","di","fi"]),("o",["<","o","m"]),("oi",["oi","d","f"]),("m",["<"]),("mi",["mi"]),("s",["s"]),("si",["=","s","si"]),("f",["d"]),("fi",["<","o","m"])]),("si",M.fromList [("<",["<","o","m","di","fi"]),(">",[">"]),("d",["oi","d","f"]),("di",["di"]),("o",["o","di","fi"]),("oi",["oi"]),("m",["o","di","fi"]),("mi",["mi"]),("s",["=","s","si"]),("si",["si"]),("f",["oi"]),("fi",["di"])]),("f",M.fromList [("<",["<"]),(">",[">"]),("d",["d"]),("di",[">","oi","mi","di","si"]),("o",["o","d","s"]),("oi",[">","oi","mi"]),("m",["m"]),("mi",[">"]),("s",["d"]),("si",[">","oi","mi"]),("f",["f"]),("fi",["=","f","fi"])]),("fi",M.fromList [("<",["<"]),(">",[">","oi","mi","di","si"]),("d",["o","d","s"]),("di",["di"]),("o",["o"]),("oi",["oi","di","si"]),("m",["m"]),("mi",["oi","di","si"]),("s",["o"]),("si",["di"]),("f",["=","f","fi"]),("fi",["fi"])])]
