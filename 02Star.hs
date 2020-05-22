


----  Advent of code, 02/2015

import Advent
import System.IO
import System.Environment


{-
	The input file must contain the dimensions of all the gift boxes in this form : LxWxH (one box per line)
	The program reads the file and turns each line of dimensions into a tuple of 3 integers, that are used to find the ft of wrapping paper.
-}


main = do 
	args <- getArgs
	contents <- readFile $ head args
	let 
		bDims = getDims (lines contents)
		toft = totalOfFeet bDims
		in putStrLn ("You need to buy a total of " ++ show toft ++ " square feet of slack.")



getDims :: [String] -> [(Integer, Integer, Integer)]
getDims [] = []
getDims (ln:ls) = let
					numz = breakBy ln 'x'
					nnumz = length numz
					rln = length (filter (\x -> x==True) (map strOrInt numz))
					nums = map (\x -> if (strOrInt x == True) then read x :: Integer else 0) numz 
					in (if (rln == nnumz && rln ==3) then [tupleIze3 nums] ++ (getDims ls) else (getDims ls))


totalOfFeet :: [(Integer, Integer, Integer)] -> Integer
totalOfFeet [] = 0
totalOfFeet (f:fs) = wrappingPaper f + totalOfFeet fs


wrappingPaper :: (Integer, Integer, Integer) -> Integer
wrappingPaper (l,w,h) = 2*l*w + 2*w*h + 2*h*l + m1*m2
					where (m1,m2) = minSide (l,w,h)


minSide :: (Integer, Integer, Integer) -> (Integer, Integer)
minSide (a, b, c) 
				| (a <= b) && (b <= c) = (a,b)
				| (a <= b) && (a <= c) && (c <= b) = (a,c)
				| (b <= a) && (a <= c) = (b,a)
				| (b <= a) && (b <= c) && (c <= a) = (b,c)
				| (c <= a) && (a <= b) = (c,a)
				| (c <= a) && (c <= b) && (b <= a) = (c,b)
