

----  Advent of code, 05/2015

import Advent
import System.IO
import System.Environment



main = do
	args <- getArgs
	contents <- readFile $ head args
	let 
	 lns = lines contents
	 lgs = countLengths lns
	 clns = zipWith (\a (b,c) -> (a,b,c)) lns lgs
	 prPr = (\(s,c,r) -> "Phrase:" ++ s ++ ", code length:" ++ (show c) ++ ", real length:" ++ (show r) ++ ".")
	 in mapM putStrLn (fmap prPr clns)


countLengths :: [String] -> [(Int,Int)]
countLengths [] = []
countLengths (x:xs) = (cl,rl) : (countLengths xs)
					where 
						rl = realStrLen x 
						cl = codeStrLen x




-- 2 is for the double quotes
codeStrLen :: String -> Int 
codeStrLen = length



--- Escaped characters


{-
	The strings we are dealing with may contain escaped characters.
	The allowable escaped characters are :	\" for double quote
											\\ for backslash
											\xHEX for a character in hex represetnation
	So, if a string contains a \, we need to check for escaped characters	
-}


-- Input strings are enclosed in double quotes, so we need to subtract 2, for Haskell has taken them as part of the string
realStrLen :: String -> Int
realStrLen [] = -2
realStrLen (l:ls) 
				| (l == '\\') = n1 + (realStrLen nls)
				| otherwise = 1 + (realStrLen ls)
				where 
					(n1,n2) = escaped (l:ls)
					tls = take n2 ls
					nls = [x | x <- ls, not (x `elem` tls)]


escaped :: String -> (Int,Int)
escaped (l:ls) = if te == True then (1,stps) else (0,stps)
              where 
              	eB = escapedBkSl ls
              	eD = escapedDQuote ls
              	eH = escapedHex ls
              	te = not ((eB == 0) && (eD == 0) && (eH == 0))
              	stps = if eB == 0 then (if eD == 0 then (if eH == 0 then 0 else 3) else 1) else 1


escapedBkSl :: String -> Int
escapedBkSl (l:_) = if b == True then 1 else 0
					 where
						b = l == '\\'


escapedDQuote :: String -> Int
escapedDQuote (l:_) = if q == True then 1 else 0
						where
							q = l == '\"' 


escapedHex :: String -> Int
escapedHex (l:ls) = if (x == True && (hU == True || hL == True)) then 1 else 0
					where
						x = l == 'x' 
						phx = take 2 ls
						hexU = ['A'..'Z'] ++ ['0'..'9']
						hexL = ['a'..'z'] ++ ['0'..'9']
						hU = length ([p | p <- phx, p `elem` hexU]) == length phx
						hL = length ([p | p <- phx, p `elem` hexL]) == length phx
						 
