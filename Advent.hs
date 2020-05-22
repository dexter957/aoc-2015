

--- Useful functions for all Stars problems

module Advent
(
sanitizeInput,
howManyDistinct,
howManyDups,
uniques,
consecsOfN,
getDigits,
strOrInt,
breakBy,
tupleIze3
)where


sanitizeInput :: Eq a => [a] -> [a] -> Bool
sanitizeInput ls ns = length pl == 0
                      where pl = [x | x <- ls, not $x `elem` ns]


howManyDistinct :: Eq a => [a] -> Int
howManyDistinct = length . dhse
                    where dhse [] = []
                          dhse (h:hs)
                               | h `elem` hs = dhse hs
                               | otherwise = [h] ++ dhse hs
                               

howManyDups :: Eq a => [a] -> Int
howManyDups = length . dhse
                    where dhse [] = []
                          dhse (h:hs)
                               | h `elem` hs = [h] ++ dhse hs
                               | otherwise = dhse hs


uniques :: Eq a => [a] -> [a]
uniques [] = []
uniques (h:hs) 
			| h `elem` hs = uniques hs 
			| otherwise = h : uniques hs 


consecsOfN :: Eq a => [a] -> Int -> [[a]]
consecsOfN [] _ = []
consecsOfN ls n = [ns] ++ consecsOfN rs n
                      where ns = take n ls
                            rs = tail ls


getDigits :: String -> String
getDigits str = [x | x <- str, x `elem` "0123456789" ] 	


strOrInt :: String -> Bool 
strOrInt si = let 
				digits = "0123456789"
				f = (\x -> if x `elem` digits then 1 else 0)
				nums = map f si 
				in sum(nums) == length si


breakBy :: String -> Char -> [String]
breakBy s c = breakBy' s c []


breakBy' :: String -> Char -> String -> [String]
breakBy' [] _ a = [a]
breakBy' (s:ss) c a 
					| (s == c) = [a] ++ (breakBy' ss c [])
					| otherwise = breakBy' ss c (a ++ [s])


tupleIze3 :: [a] -> (a, a, a)
tupleIze3 l = (head l, head (tail l), head (reverse l) )