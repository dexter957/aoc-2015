

----  Advent of code, 11/2015

import Advent


main = do
	putStrLn "Insert old password"
	oldPwd <- getLine
	let
	 vl = ['a'..'z']
	 ans = case (sanitizeInput oldPwd vl) of
	 		True -> "Your new password is:" ++ nextPwd oldPwd 
	 		False -> "Invalid input passwrod"
	 in putStrLn ans



nextPwd :: String -> String
nextPwd cur 
			| ((a == False) && b && c) == True = np
			| otherwise = nextPwd np
			where
				np = changePass cur
				a = forbChars np
				b = triads np
				c = sDiads np


changePass :: String -> String 
changePass = reverse . incrLettr . reverse



forbChars :: String -> Bool
forbChars [] = False
forbChars pwd = let 
					forbs = "ilo"
					in (foldl (\acc x -> if x `elem` forbs then acc+1 else acc) 0 pwd) > 0


-- Before calling below function, one must reverse the string to be given as input.
-- After calling the function, the correct result is the function result, reversed
incrLettr :: String -> String 
incrLettr [] = []
incrLettr ('z':ls) = 'a' : incrLettr ls
incrLettr (l:ls) = n : ls
				where 
					alb = [l..'z']
					n = head (tail alb)




triads :: String -> Bool
triads pwd = (length t) > 0
			where 
				talb = consecsOfN ['a'..'z'] 3
				tpwd = consecsOfN pwd 3
				t = [tr | tr <- tpwd, tr `elem` talb, (length tr) >=3]


sDiads :: String -> Bool
sDiads pwd = ((length sdds) - (howManyDups sdds)) >= 2
			where 
				dds = consecsOfN pwd 2
				sdds = [d | d <- dds, (length d) == 2, (howManyDistinct d) == 1]