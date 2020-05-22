


----  Advent of code, 01/2015

import Advent


main = do
        putStrLn "Insert Santa's directions"
        floors <- getLine
        let vl = "()"
            ansDirs = case (sanitizeInput floors vl) of
                        True   -> "Santa must go to floor " ++ show (floorForSanta floors)
                        False  -> "Invalid directions"
         in putStrLn ansDirs


floorForSanta :: String -> Int
floorForSanta ins = foldr (\x acc -> if x == '(' then acc +1 else acc -1) 0  (reverse ins)
