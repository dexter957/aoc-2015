

----  Advent of code, 14/2015

import Advent
import System.IO
import System.Environment


{-
	Input should be given like this:
	Comet,10s,126s,16km/s
	Dancer,11s,162s,16km/s
-}


main = do
	args <- getArgs
	contents <- readFile $ head args
	let
		lns = lines contents
		race = map parseDeer lns
		tot = 2503
		ans = raceDeers race tot
		in putStrLn ans



raceDeers :: [(String,[Integer])] -> Integer -> String
raceDeers rc tot = raceDeers' rc 0 tot



raceDeers' :: [(String,[Integer])] -> Integer -> Integer -> String
raceDeers' [] _ tot = ""
raceDeers' (r:rs) m tot
						| (dst >= m) && (m == 0) = wn1phr ++ (raceDeers' rs dst tot)
						| (dst >= m) && (m > 0) = wn2phr ++ (raceDeers' rs dst tot)
						| otherwise = raceDeers' rs m tot
						where 
							name = fst r
							specs = snd r
							dst = deerDis (head specs) (head (tail specs)) (head (reverse specs)) tot
							wn1phr = name ++ " is winning, having travelled " ++ show dst ++ " km."
							wn2phr = "\n" ++ name ++ " is also winning"



parseDeer :: String -> (String,[Integer])
parseDeer specs = let
					toks = breakBy specs ','
					sps = map getDigits toks
					rsps = map (\x -> if ((length x) > 0) then read x :: Integer else -1) sps
					name = head toks
					nsps = take 3 (tail rsps)
					in (name,nsps)
				


deerDis :: Integer -> Integer -> Integer -> Integer -> Integer
deerDis r s h t = rt * h
				where 
					rt = deer r s t


-- Returns total running seconds. r: running time, s: rest time, t: total time
deer :: Integer -> Integer -> Integer -> Integer
deer r s t = let
				pr = t `div` (r + s)
				lf = t `mod` (r + s)
				pl = if (lf > r) then r else lf
				in pr * r + pl




