
----  Advent of code, 05/2015

import Advent

main = do
          putStrLn "Insert Santa's strings"
          hs <- getLine
          case (nice hs) of
              True -> putStrLn "Nice"
              False -> putStrLn "Naughty"



nice :: String -> Bool
nice sen = (vowelsNice sen) && (consecsNice sen) && ((not . forbidden) sen)


vowelsNice :: String -> Bool
vowelsNice sen = (length $ vowels) > 2
                  where vowels = [v | v <- sen, v `elem` "aeiouy"]


consecsNice :: String -> Bool
consecsNice sen = True `elem` tfs
                  where dupls = (\x -> (howManyDups x) == 1)
                        diads = consecsOfN sen 2
                        tfs = map dupls diads


forbidden :: String -> Bool
forbidden sen = let forbs = ["ab","cd","pq","xy"]
                    diads = consecsOfN sen 2
                    chker = (\x -> x `elem` forbs)
                    tfs = map chker diads
                    in True `elem` tfs



