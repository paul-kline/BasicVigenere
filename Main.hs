module Main where
import BasicVigenere

import Criterion.Main 
import Text.Read 
import Data.List
main = do 
  putStrLn " What would you like to do?"
  putStrLn "\t1. Run all benchmarks (assuming cmd arg: \"--output OUTPUT.html\")"
  putStrLn "\t2. Decrypt"
 
  resp <- getLine
  case resp of 
       "1" -> do 
            defaultMain [
               bgroup "bruteForceAttacks" 
                 [ bench ("cypher1: keylen:" ++ (show k1) ++ ", WordLen: " ++ (show c1)) $ nfIO (crackOpt1 p1 k1 c1)	
		 , bench ("cypher2: keylen:" ++ (show k2) ++ ", WordLen: " ++ (show c2)) $ nfIO (crackOpt1 p2 k2 c2)
		 , bench ("cypher3: keylen:" ++ (show k3) ++ ", WordLen: " ++ (show c3)) $ nfIO (crackOpt1 p3 k3 c3)
		 , bench ("cypher4: keylen:" ++ (show k4) ++ ", WordLen: " ++ (show c4)) $ nfIO (crackOpt1 p4 k4 c4)
		 , bench ("cypher5: keylen:" ++ (show k5) ++ ", WordLen: " ++ (show c5)) $ nfIO (crackOpt1 p5 k5 c5)
		 , bench ("cypher6: keylen:" ++ (show k6) ++ ", WordLen: " ++ (show c6)) $ nfIO (crackOpt1 p6 k6 c6)
		 ]
               ]
       "2" -> do 
	 putStrLn "Enter the CypherText:"
	 c <- getLine
	 putStrLn "Key length (press enter if unknown):"
         keyLenS <- getLine
         putStrLn "1st Word Length (press enter if unknown):"
	 wordLenS <- getLine
         let keyLenM = readMaybe keyLenS :: Maybe Int 
             wordLenM = readMaybe wordLenS :: Maybe Int 
             f = crackOpt1 c
         case (keyLenM,wordLenM) of 
	      (Just kl, Just wl) -> do 
		f kl wl
		return () 
	      (Nothing , Just wl) -> do 
		putStrLn "Enter the min and max KEY length separated with a space. example: '1 10':"
		minmaxstr <- getLine 
		let [minS,maxS] = words minmaxstr 
		let minK = read minS :: Int 
		    maxK = read maxS :: Int 
		    mods = map ((flip f) wl) [minK..maxK]
		sequence mods
		return () 
	      (Just kl, Nothing) -> do 
		putStrLn "Enter the min and max WORD length separated with a space. example: '1 10':"
		minmaxstr <- getLine 
		let [minS,maxS] = words minmaxstr 
		let minw = read minS :: Int 
		    maxw = read maxS :: Int 
		    mods = map (f kl) [minw..maxw]
		sequence mods
		return () 
	      (Nothing, Nothing) -> do 
		putStrLn "Enter the min and max KEY length separated with a space. example: '1 10':"
		minmaxstr1 <- getLine 
		let [minS1,maxS1] = words minmaxstr1 
		let minK = read minS1 :: Int 
		    maxK = read maxS1 :: Int 
		putStrLn "Enter the min and max WORD length separated with a space. example: '1 10':"
		minmaxstr2 <- getLine 
		let [minS2,maxS2] = words minmaxstr2 
		let minw = read minS2 :: Int 
		    maxw = read maxS2 :: Int
		    allPoss = [(k,w) | k <- [minK..maxK], w <- [minw..maxw]]
		    mods = map (\(k,w) -> f k w) allPoss 
		sequence mods 
		return ()