
----  Advent of code, 17/2015

import Advent
import Data.List
import System.IO
import System.Environment

{-
	Input of total capacity to be stored, as well as the available containers, will be given as command line arguments.
	Specifically, flag -c refers to the total capacity to be stored
				  flag -s refers to the available containers
				 i.e -c 25 -s 20 15 10 5 5

-}


main = do 
		args <- getArgs
		let
			argus = intercalate " " args
			(ok, tot, cons) = processArgs argus 
			out = if ok == True then containers cons tot else []
			in print out



containers :: [Integer] -> Integer -> [[Integer]]
containers bts c = let
					sbts = subsequences bts
					f = (\x -> sum x == c)
					in filter f sbts





processArgs :: String -> (Bool, Integer,[Integer])
processArgs arg = (tf, tot, cons)
				where 
					tot = getTotal arg 
					cons = getCons arg 
					tf = not (tot == 0 || cons == [])
 

getCons :: String -> [Integer]
getCons [] = []
getCons ('-':as) 
				| head as == 's' = toNumz snz 
				| otherwise = getCons as 
				where 
					snz = (words . tail) as
getCons (_:as) = getCons as 




toNumz :: [String] -> [Integer]
toNumz [] = []
toNumz (a:as) = if is == True then [read a :: Integer] ++ toNumz as else [] ++ toNumz as
				where 
					is = strOrInt a




getTotal :: String -> Integer
getTotal as = let 
				sua = getCFlag' as False
				pnum = (head . words) sua
				is = strOrInt pnum
			  	in (if is == True then (read pnum :: Integer) else 0)


getCFlag' :: String -> Bool -> String 
getCFlag' [] _ = []
getCFlag' ('-':as) yn 
					| ((head as == 'c') && (yn == False)) = getCFlag' (tail as) True
					| otherwise = getCFlag' as False
getCFlag' (a:as) True = [a] ++ getCFlag' as True
getCFlag' (a:as) _ = getCFlag' as False

















