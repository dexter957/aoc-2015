


----  Advent of code, 19/2015

import Advent
import Data.Char
import System.IO
import System.Environment




{-
	The input file should contain the calibration, the way it is demonstrated in the exaple:
	H => HO
	H => OH
	O => HH
    The input file is read, so the fusion can be calibrated.
    When the calibration has occurred, the replacements are generated
	Calibration 
	[ (L, [R1,R2,..,Rn]) ] where L is the specific letter that can be replaced, and Rn are the replacement letters
-}


main = do
	args <- getArgs
	contents <- readFile $ head args
	putStrLn "Insert input molecule" 
	mol <- getLine
	if (notAtExit mol) == True
		then do 
			putStrLn "Goodbye"
		else do 
			let lns = lines contents
			let cal = calibration lns 
			let mols = molecules mol cal
			let ans = "There are " ++ show mols ++ " replacement molecules"
			putStrLn ans
			main 
		 		



notAtExit :: String -> Bool
notAtExit str = caps == "EXIT" || lows =="exit"
			where
				caps = map toUpper str
				lows = map toLower str


calibration :: [String] -> [(Char,[String])]
calibration s = let
					j = map calibrate s
					aa = uniques ([(fst x) | x <- j])
					f = (\a -> (a, flatten ([(snd x) | x <-j, (fst x) == a])))
					in map f aa


calibrate :: String -> (Char,[String])
calibrate s = let 
				ws = words s
				o = "=>"
				cws = [x | x <- ws, not (o == x)]
				c = head (head cws)
				sr = tail cws
				in (c,sr)


molecules :: String -> [(Char,[String])] -> Int
molecules x y = howManyDistinct (replace x y)


replace :: String -> [(Char,[String])] -> [String]
replace _ [] = []
replace s (r:rs) = (allReplaces s r) ++ replace s rs  


allReplaces :: String -> (Char,[String]) -> [String]
allReplaces s (c,r) = let
						l = [(s,c,x) | x <- r ]
						f = (\(x,y,z) -> rdReplace x y z [])
						in flatten (map f l)


rdReplace :: String -> Char -> String -> String -> [String]
rdReplace [] c r a = []
rdReplace (s:ss) c r a 
						| (s == c) = (a ++ r ++ ss) : (rdReplace ss c r (a ++ [s]))
						| otherwise = rdReplace ss c r (a ++ [s])


flatten :: [[String]] -> [String]
flatten [] = []
flatten (s:ss) = s ++ flatten ss 