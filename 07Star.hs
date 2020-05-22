


----  Advent of code, 07/2015

import Advent
import qualified Data.Map as M







fromBinaryTo10 :: [Int] -> Int
fromBinaryTo10 bin = sum $ multLists pow2 bin
                      where
                            n = length bin
                            bins = reverse [0..n-1]
                            pow2 = (2^) <$> bins


multLists :: [Int] -> [Int] -> [Int]
multLists [] [] = []
multLists (n:ns) (b:bs) = [n*b] ++ multLists ns bs

{-
  Έχουμε 16 bits, άρα μπορούμε να έχουμε μέχρι 16 θέσεις στη λίστα, με εκθέτη 0-15
  Η λογική με την οποία γίνεται η μετατροπή από δεκαδικό σε δυαδικό, ακολουθεί την παρακάτω αναδρομική σκέψη:
  Βρίσκω τη δύναμη του 2 (ν) που είναι αμέσως μικρότερη ή ίση με τον αριθμό που θέλω να μετατρέψω
  Αν είναι ίση, έχω τον αριθμό μου ( 1 * 2^ν)
  Αν όχι αφαιρώ τη δύναμη αυτήν, κι επαναλαμβάνω μέχρι να φτάσω στο 0
-}

from10ToBinary :: Int -> [Int]
from10ToBinary ten = let 
						anlpows = reverse $ from10ToBinary1 ten
						pow2s = (2^) <$> [0..15] 
						in reverse $ onesORzeros anlpows pow2s 


from10ToBinary1 :: Int -> [Int]
from10ToBinary1 0 = []
from10ToBinary1 ten = num : from10ToBinary1 (ten - num)
					  where num = from10ToBinary2 ten 


from10ToBinary2 :: Int -> Int
from10ToBinary2 ten = let 
						pows = [0..15]
						pow2 = (2^) <$> pows 
						in head $ foldl (\acc x -> if (min ten x)== x then x : acc else acc ) [] pow2 
					  

onesORzeros :: [Int] -> [Int] -> [Int]
onesORzeros _ [] = []
onesORzeros [] (b:bs) = [0] ++ onesORzeros [] bs  
onesORzeros all@(t:ts) (b:bs) = if (t == b) then ([1] ++ onesORzeros ts bs) else ([0] ++ onesORzeros all bs) 



-------------------------  BITWISE OPERATIONS -----------------------------


bitAND :: [Int] -> [Int] -> [Int]
bitAND [] [] = []
bitAND (x:xs) (y:ys) = [x*y] ++ (bitAND xs ys)


bitOR :: [Int] -> [Int] -> [Int]
bitOR [] [] = []
bitOR (1:xs) (_:ys) = [1] ++ (bitOR xs ys) 
bitOR (_:xs) (1:ys) = [1] ++ (bitOR xs ys)
bitOR (_:xs) (_:ys) = [0] ++ (bitOR xs ys)


bitNOT :: [Int] -> [Int]
bitNOT [] = []
bitNOT (1:xs) = [0] ++ bitNOT xs 
bitNOT (0:xs) = [1] ++ bitNOT xs 


bitXOR :: [Int] -> [Int] -> [Int]
bitXOR [] [] = []
bitXOR (1:xs) (0:ys) = [1] ++ (bitXOR xs ys) 
bitXOR (0:xs) (1:ys) = [1] ++ (bitXOR xs ys)
bitXOR (_:xs) (_:ys) = [0] ++ (bitXOR xs ys)


bitLSHIFT :: [Int] -> Int -> [Int]
bitLSHIFT bs n = let 
					k = length bs - n 
					end = take n bs 
					start = reverse $ take k (reverse bs)
					in start ++ end 


bitRSHIFT :: [Int] -> Int -> [Int]	
bitRSHIFT bs n = let  
					k = length bs - n 
					start = reverse $ take n (reverse bs)
					end = take k bs 
					in start ++ end

 

---------- program parser ----------
    

type Vars = M.Map String Int 

data Operator = AND | OR | XOR | LSHIFT | RSHIFT | ASSIGN | NOT | ERROR deriving (Show, Eq)

data Instruction =   Strer String
				   | Inter Int 	
				   | UOp Operator Instruction				--- Unary Operator
				   | BOp Operator Instruction Instruction   --- Binary Operator
				   deriving (Show, Eq)





------ program evaluation -------

--- Relative order of precedence NOT > XOR > AND > OR


{-
	We use a map to keep the values of variables. A key-value pair is the variable name, and its numeric value
-}


evaluateBinL :: Instruction -> Vars -> (Int,Vars) 
evaluateBinL (Inter x) m = (x,m)
evaluateBinL (BOp ASSIGN y (Strer x)) m = let 
											(num, _) = evaluateBinL y m
											nm = M.insert x num m 
											in (num,nm)  
evaluateBinL (BOp OR x y) m = let 
								inBitx = from10ToBinary $ fst (evaluateBinL x m)
								inBity = from10ToBinary $ fst (evaluateBinL y m)
								orr = bitOR inBitx inBity 
								in (fromBinaryTo10 orr,m) 
evaluateBinL (BOp AND x y) m = let
								 inBitx = from10ToBinary $ fst (evaluateBinL x m)
								 inBity = from10ToBinary $ fst (evaluateBinL y m)
								 andd = bitAND inBitx inBity 
								 in (fromBinaryTo10 andd,m)
evaluateBinL (BOp RSHIFT x y) m = let 
									inBitx = from10ToBinary $ fst (evaluateBinL x m) 
									(nY,_) = evaluateBinL y m
								   	rshiftt = bitRSHIFT inBitx nY
								    in (fromBinaryTo10 rshiftt, m)
evaluateBinL (BOp LSHIFT x y) m = let 
									inBitx = from10ToBinary $ fst (evaluateBinL x m)
									(nY,_) = evaluateBinL y m
									lshiftt = bitLSHIFT inBitx nY
									in (fromBinaryTo10 lshiftt, m)
evaluateBinL (BOp XOR x y) m = let 
								inBitx = from10ToBinary $ fst (evaluateBinL x m)
								inBity = from10ToBinary $ fst (evaluateBinL y m)
								xorr = bitXOR inBitx inBity
								in (fromBinaryTo10 xorr,m) 
evaluateBinL (UOp NOT x) m = let 
								inBit = from10ToBinary $ fst (evaluateBinL x m)
								nott = bitNOT inBit 
								in (fromBinaryTo10 nott,m)		
evaluateBinL (Strer x) m = let 
							 n = M.lookup x m 
							 in case n of 
							 	   Just n -> (n,m) 
							 	   Nothing -> (0,m)

 



--------  Program Parser --------



parseLine :: String -> Instruction
parseLine line = program 
				where 
					wrds = words line 
					nl = length wrds 
					nwds = [1..nl]
					f = (\a b -> (a,b))
					ins = zipWith f nwds wrds
					fsts = [(l,w) | (l,w) <- ins, w `elem` ["->","AND","XOR","OR","RSHIFT","LSHIFT","NOT"]]
					snds = [p | p <- ins, not (p `elem` fsts)]
					sfsts = M.toDescList (M.fromList fsts)
					program = parseBinPrg (sfsts ++ snds) 







parseBinPrg :: [(Int,String)] -> Instruction
parseBinPrg all@((l,"->"):_) = (BOp ASSIGN instr1 instr2)
							    where
									  nxt = filter (\(n,s) -> n > l) all
									  prv = filter (\(n,s) -> n < l) all
									  instr1 = parseBinPrg prv
									  instr2 = parseBinPrg nxt 
parseBinPrg all@((l,"AND"):_) = (BOp AND instr1 instr2)
								 where 
								 	   nxt = filter (\(n,s) -> n > l) all
								 	   prv = filter (\(n,s) -> n < l) all
								 	   instr1 = parseBinPrg prv
							 		   instr2 = parseBinPrg nxt 
parseBinPrg all@((l,"OR"):_) = (BOp OR instr1 instr2)
								where 
									   nxt = filter (\(n,s) -> n > l) all
									   prv = filter (\(n,s) -> n < l) all
									   instr1 = parseBinPrg prv
									   instr2 = parseBinPrg nxt
parseBinPrg all@((l,"XOR"):_) = (BOp XOR instr1 instr2)
								 where 
								 	   nxt = filter (\(n,s) -> n > l) all
								 	   prv = filter (\(n,s) -> n < l) all 								 
								 	   instr1 = parseBinPrg prv 
							 		   instr2 = parseBinPrg nxt
parseBinPrg all@((l,"RSHIFT"):_) = (BOp RSHIFT instr1 instr2)
								   where
								   		 nxt = filter (\(n,s) -> n > l) all
								   		 prv = filter (\(n,s) -> n < l) all
								   		 instr1 = parseBinPrg prv
								   		 instr2 = parseBinPrg nxt	
parseBinPrg all@((l,"LSHIFT"):_) = (BOp LSHIFT instr1 instr2)								   		 
								   where
								   	     nxt = filter (\(n,s) -> n > l) all
								   	     prv = filter (\(n,s) -> n < l) all
								   	     instr1 = parseBinPrg prv
								   	     instr2 = parseBinPrg nxt
parseBinPrg all@((l,"NOT"):_) = (UOp NOT instr)
								 where 
								 	   nxt = filter (\(n,s) -> n > l) all 
									   instr = parseBinPrg nxt
parseBinPrg ((_,x):_) = if ns == False then (Strer x) else (Inter i) 
								where 
									  ns = strOrInt x 
									  i = read x :: Int 





-----  program execution ----

executeLine :: String -> [(String, Int)] -> (Int,[(String,Int)]) 
executeLine line vrs = let 
					    initial = ("",0)
					    variables = M.fromList vrs
					    prg = parseLine line
					    (res,vs) = evaluateBinL prg variables
					    nvs = [x | x <- (M.toList vs), not (x == initial)] 
						in (res,nvs)



