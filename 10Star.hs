

----  Advent of code, 10/2015

import Advent


main = do
		putStrLn "Insert puzzle input:"
		ins <- getLine
		putStrLn "Insert times of application:"
		tms <- getLine
		let 
			f = spellGrps . numGrps
			vtms = strOrInt tms
			times = if vtms == True then read tms :: Integer else 10 
			ans = length (nTimesApply f ins times)
			in putStrLn ("The length of the string, after applying the function " ++ show times ++ " times, is " ++ show ans) 




nTimesApply :: (a -> a) -> a -> Integer -> a
nTimesApply f n 1 = f n
nTimesApply f n i = nTimesApply f (f n) (i - 1)


splNGrp :: String -> String
splNGrp = spellGrps . numGrps


numGrps :: String -> [(Integer, Char)]
numGrps str = let
				l1 = numGrps' str 1 ' '
				(h1,h2) = head l1
				in (if h1 == 1 then tail l1 else ([(h1-1,h2)] ++ (tail l1))) 


numGrps' :: String -> Integer -> Char -> [(Integer, Char)]
numGrps' [] i p = [(i,p)]
numGrps' (c:cs) i p
					| (c == p) = numGrps' cs (i+1) p
					| otherwise = [(i,p)] ++ (numGrps' cs 1 c)


spellGrps :: [(Integer,Char)] -> String
spellGrps l = foldl (++) [] ln 
			where
				f = (\(a,b) -> show a ++ [b])
				ln = map f l
