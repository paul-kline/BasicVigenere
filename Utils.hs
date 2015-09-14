module Utils where


import BasicVigenere
import qualified Data.Set as S
import qualified Data.List as L
import qualified Data.Map.Lazy as M
import System.IO
import qualified Data.Text as T
import Control.Monad (join)
replace :: (Eq a) => a -> a -> [a] -> [a] 
replace _ _ [] = []
replace x y (a:xs) = if x == a then 
                       y: (replace x y xs)
                     else 
                       a:(replace x y xs)
                       
                       
                       
str4="jrgdgidxgqanngzgtgttsitgjranmnoeddiomnwjrajvksexjmdxkmnwjrgmttgdtgognjajmzgovgkinlaqgtjamnxmsmjjrgkojtgnwjrgnjrgvattmgtawamnojjrgwizgtnsgnjibabgu"
str4' = ( replace 'g' 'E' (replace 'j' 'T' (replace 'r' 'H' str4)))

mydomain = [ x | x <- ['a'..'z'], x `notElem` "gjr"]
myrange =  [y | y <-['A'..'Z'], y `notElem` "THE" ]
allOrdersRange = L.permutations myrange 

decode :: [Char] -> [Char] -> String -> String 
decode [] _ str = str 
decode (x:xs) (y:ys) str = replace x y (decode xs ys str)

decodeAll :: [Char] -> [[Char]] -> String -> [T.Text]
decodeAll dom rangLS str = L.map (\x ->T.pack (decode dom x str)) rangLS

hugeDic :: IO (S.Set T.Text)
hugeDic = do
  handle <- openFile "dict.txt" ReadMode
  contents <- hGetContents handle 
  return $ S.fromList $ map T.pack $ lines contents

plainify :: [(Char,Char)] -> String -> String 
plainify ls str = foldr (\(x,y) acc -> replace x y acc) str ls 


excludingTHEmapping = [(x,y) | x <- mydomain, y <- myrange ]


permutation = sequence $ take 23 $ repeat excludingTHEmapping 

plaintexts = map (\mappingLS -> foldr (\(x,y) acc -> replace x y acc) str4' mappingLS) permutation

--main = myMain 


myMain = do 
  dic <- hugeDic
  let allPlainTexts = decodeAll mydomain allOrdersRange str4' -- ::[Text] 
  let pairs = L.map (\plaintext -> (S.foldr (\word acc-> if word `T.isInfixOf` plaintext then acc + 1 else acc) 0 dic , plaintext)) allPlainTexts 
  writeTo "problem4Outputs.txt" pairs
  sequence $ map print $ pairs 

myMain2 = do 
  dics <- sequence $ map (\i -> dicHashTree2 i) [1..15]
  let dics' = zip [1..15] dics 
  let dicMap = M.fromList dics'
  let allPlainTexts = decodeAll mydomain allOrdersRange str4' -- ::[Text] 
  --let pairs = L.map (\plaintext -> (S.foldr (\word acc-> if word `T.isInfixOf` plaintext then acc + 1 else acc) 0 dic , plaintext)) allPlainTexts
  let realSentences = filter ((isSentence dicMap) . T.unpack) allPlainTexts
  writeTo "problem4OutputsISSENTENCE.txt" realSentences

  
writeTo :: (Show a) => String -> [a] -> IO () 
writeTo p ls= do 
  withFile p WriteMode (\h -> do 
			 r <- sequence $ map (\x -> hPutStrLn h (show x)) ls
			 return () ) 
			 
--  -}



isSentence :: M.Map Int (HashTree Char Char) -> String -> Bool 
isSentence _ [] = True
isSentence m p = 
  let pairs = map (\i -> let w = take i p in 
                           (i,w)) [1..15]
      icare = filter (\(i,w) -> case M.lookup i m of 
				     Nothing -> False 
				     Just dic -> 
				       myelem w dic 
				       ) pairs in 
  if (length icare) == 0 then 
    False 
  else
    or (map (\(i,w) -> isSentence m (drop i p)) icare)
		     
 




























