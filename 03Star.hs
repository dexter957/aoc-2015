

----  Advent of code, 03/2015

import Advent


main = do
         putStrLn "Insert Santa's directions"
         hs <- getLine
         let vl = "^><v"
             sepHs = case (sanitizeInput hs vl) of
                       True -> "There are " ++ show (houses hs) ++ " distinct houses"
                       False -> "Invalid directions"
             houses = howManyDistinct . folDirs
          in putStrLn sepHs


folDirs :: String -> [(Integer,Integer)]
folDirs drs = let  rh = head . reverse
                   fol = \d (x,y) ->
                       case d of
                         '^' -> (x,y+1)
                         'v' -> (x,y-1)
                         '>' -> (x+1,y)
                         '<' -> (x-1,y)
                  in foldr (\d acc-> acc ++ [fol d (rh acc)]) [(0,0)] (reverse drs)
