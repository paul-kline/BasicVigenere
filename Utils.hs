module Utils where


import BasicVigenere
import qualified Data.Set as S
import qualified Data.List as L
import qualified Data.Map.Lazy as M
import System.IO
import qualified Data.Text as T
import Control.Monad (join)
import qualified Data.HashMap as HM
replace :: (Eq a) => a -> a -> [a] -> [a] 
replace _ _ [] = []
replace x y (a:xs) = if x == a then 
                       y: (replace x y xs)
                     else 
                       a:(replace x y xs)
                       
replace' :: (Eq a) => [(a,a)] -> [a] -> [a]
replace' ls str = foldr (\(x,y) acc-> replace x y acc) str ls  
                       
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

getHugeDicTree = do 
  dics <- sequence $ map (\i -> dicHashTree2 i) [1..15::Int]
  let dics' = zip [1..15::Int] dics 
  return $ M.fromList dics'
myMain2 = do 
  dics <- sequence $ map (\i -> dicHashTree2 i) [1..15]
  let dics' = zip [1..15] dics 
  let dicMap = M.fromList dics'
  let allPlainTexts = decodeAll mydomain allOrdersRange str4' -- ::[Text] 
  --let pairs = L.map (\plaintext -> (S.foldr (\word acc-> if word `T.isInfixOf` plaintext then acc + 1 else acc) 0 dic , plaintext)) allPlainTexts
  let realSentences = filter ((isSentence dicMap) . T.unpack) allPlainTexts
  writeTo "problem4OutputsISSENTENCE.txt" realSentences
myMain3 = do 
  dic <- hugeDic
  let mappins = getMappings mappings 
  let str = str4
  let plaintexts = map ( (flip replace') str) mappins 
  --sequence $ map print $ take 1 plaintexts 
  writeTo "problem4SHRUNKENRANGES_mappingBeginning.txt" plaintexts
   
  
writeTo :: (Show a) => String -> [a] -> IO () 
writeTo p ls= do 
  withFile p WriteMode (\h -> do 
			 r <- sequence $ map (\x -> hPutStrLn h (show x)) ls
			 return () ) 
			 
--  -}



str3 = encrypt "pbeguuymiqicuufguuyiqguuyqcuivfiqguuyqcuqbemevp" "A" 
str3lower = "pbeguuymiqicuufguuyiqguuyqcuivfiqguuyqcuqbemevp"
str5 = encrypt "ejitpspawaqlejitaiulrtwllrflrllaoatwsqqjatgackthlsiraoatwlplqjatwjufrhlhutsqataqitatsaittkstqfjcae" "A" 

{-
f1 = (\a -> map (encrypt a) [[x] | x <-['A'..'Z']])
f2 = (\a -> map (encrypt a) [[x,y] | x <-['A'..'Z'], y <- ['A'..'Z']])
f3 = (\a -> map (encrypt a) [[x,y,z] | x <-['A'..'Z'], y <- ['A'..'Z'], z <- ['A'..'Z']])
f4 = (\a -> map (encrypt a) [[x,y,z,w] | x <-['A'..'Z'], y <- ['A'..'Z'], z <- ['A'..'Z'], w <- ['A'..'Z']])
f5 = (\a -> map (encrypt a) [[x,y,z,w,p] | x <-['A'..'Z'], y <- ['A'..'Z'], z <- ['A'..'Z'], w <- ['A'..'Z'], p <- ['A'..'Z']]) --this is too big and dies. 
-}

--jrgdgidxgqanngzgtgttsitgjranmnoeddiomnwjrajvksexjmdxkmnwjrgmttgdtgognjajmzgov
mappings = [
             ('j',"T")
           , ('r',"H")
           , ('g',"E")
           , ('d', "LDCRH")  --don't think it's 'U' here.
           , ('i', "AUDLRC")
           , ('x', "CUMWF")
           , ('q', "NPBVKG")
           , ('a', "NIOSH")
           , ('n', "AOINS")
           ]
       {-
           , ('n',"AOINSRDL")
           , ('t',"OSFLM") --since tt is most common double in problem 4, (3 times) most common double letters are [SS,EE,TT,FF,LL,MM,OO]
           , ('m',"IONSRDLA")
           , ('a',"NAOISRDLUC")
           --r is done
           , ('d', "NSHRDLUCMWFYG") --d is 4.1% frequent. taking Eng frequencies 6.7-2.0%
           , ('i', "SHRDLUCMWFYGN") --d is 4.1% frequent. taking Eng frequencies 6.7-2.0%
           , ('o', "HRDLUCMWFYGNS") --d is 4.1% frequent. taking Eng frequencies 6.7-2.0%
           , ('w', "SHRDLUCMWFYGPB") --w is 3.4%   taking 6.3-1.5
           , ('k', "KVBPGYFWMCULDR") -- k is 2.8%  taking 0.8-6.0%
           , ('s', "VBPGYFWMCULDRK") -- k is 2.8%  taking 0.8-6.0%
           , ('x', "BPGYFWMCULDRKV") -- k is 2.8%  taking 0.8-6.0%
           , ('v', "JXKVBPGYFWMCUL") -- v is 2.1%  taking 0.2-4.0
           , ('b', "XKVBPGYFWMCUJ") --b is 1.4%    taking 0.2-2.8%
           , ('e', "KVBPGYFWMCUJX") --b is 1.4%    taking 0.2-2.8%
           , ('q', "VBPGYFWMCUJXK") --b is 1.4%    taking 0.2-2.8%
           , ('u', "PGYFJXKVB")
           
           ]-}
mappings' = M.fromList (map (\(x,y) -> (x, S.fromList y)) mappings) 

getMappings :: [(Char,[Char])] -> [ [(Char,Char)] ] 
getMappings ls = do 
   let pp = map (\(x,ys) -> [(x,y) | y <- ys]) ls 
   let hasRepeats = sequence pp --meaning that [('a','B'), ('c', 'B')] is in here. we want a 1 to 1 mapping so we remove those in the next line. 
   filter isUniqueMapping hasRepeats

isUniqueMapping :: (Eq a) => [(a,a)] -> Bool 
isUniqueMapping ls = let rang = map snd ls in 
                         (length rang) == (length (L.nub rang))
   

--subCracker :: M.Map Char (S.Set Char) -> 


col :: String -> Int -> IO () 
col str i = do 
   let ls = mySplit str i 
       s  = map putStrLn ls 
   sequence s      
   return ()
   
mySplit ::[a] -> Int -> [[a]]
mySplit [] _ = []
mySplit ls i = (take i ls): (mySplit (drop i ls) i)
















