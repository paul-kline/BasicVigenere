module BasicVigenere where

import Data.Char
import System.IO
import qualified Data.Set as S 
import qualified GHC.Conc as C
--import qualified Data.HashTable.ST.Basic as HT
--import qualified Data.HashTable.IO as HIO
--import qualified Data.HashSet as HS 
import qualified Data.HashMap as HM
import Data.Hashable (Hashable)
import qualified Data.Map.Lazy as M


type CypherText = String
type Key = String 
type PlainText = String 




encrypt :: PlainText -> Key -> CypherText
encrypt p k = map encrypt' $ zip (map toUpper p) $ (concat . repeat) (map toUpper k) 

decrypt :: CypherText -> Key -> PlainText
decrypt c k = map decrypt' $ zip c ((concat . repeat) k)
--ASSUMED ALL UPPERCASE 
--'A' = 65
--'Z' = 90
decrypt_ :: CypherText -> Key -> (PlainText,Key)
decrypt_ c k = (decrypt c k,k)

encrypt' :: (Char,Char) -> Char 
encrypt' (p,k) = fromNum $ ((toNum p) + (toNum k)) `mod` 26

decrypt' :: (Char,Char) -> Char 
decrypt' (c,k) = fromNum $ ((toNum c) - (toNum k)) `mod` 26

toNum :: (Num a) => Char -> a
toNum c = (fromIntegral (fromEnum (toUpper c))) - 65

fromNum :: (Enum a) => a -> Char 
fromNum n = toEnum $ (fromEnum n) + 65 


getKeys :: Int -> [String]
getKeys i = sequence $ take i $ repeat ['A'..'Z']

dicSet :: Int ->  IO (S.Set String)
dicSet i = do 
  ls <- dicList i  
  return $ S.fromList ls 

dicList :: Int -> IO [String] 
dicList i = do
  handle <- openFile "dict.txt" ReadMode
  contents <- hGetContents handle 
  return $ filter (\x -> (length x) == i) $ lines contents

dicList2 :: Int -> IO [String] 
dicList2 i = do
  handle <- openFile "dict2.txt" ReadMode
  contents <- hGetContents handle 
  return $ filter (\x -> (length x) == i) $ lines contents  
  {-
dicHashSet :: Int -> IO (HS.HashSet String)
dicHashSet i = do 
  ls <- dicList i 
  return $ HS.fromList ls 
-}

data HashTree k v = HashNode (HM.Map k (HashTree k v ))
                  | HashLeaf deriving (Show, Eq)
mylookup ::(Data.Hashable.Hashable k, Ord k) => k -> HashTree k v -> Maybe (HashTree k v) 
mylookup _ HashLeaf = Nothing
mylookup k (HashNode m) = HM.lookup k m 

myelem ::(Data.Hashable.Hashable a, Ord a)=> [a] -> HashTree a a -> Bool
myelem [] _ = True 
myelem(x:xs) t = case mylookup x t of 
  Nothing -> False
  Just r  -> myelem xs r   
--k would be the second to last letter, v would be the last. 

dicHashTree :: Int -> IO (HashTree Char Char) 
dicHashTree i = do 
  ls <- dicList i 
  return $ createHashTree ls 

dicHashTree2 :: Int -> IO (HashTree Char Char) 
dicHashTree2 i = do 
  ls <- dicList2 i 
  return $ createHashTree ls
  
createHashTree :: [String] -> HashTree Char Char 
createHashTree  [[]] = HashLeaf 
createHashTree ls = let headtails = map (\w -> (head w, tail w)) ls 
                        mymap = foldr (\(k,rest) acc -> HM.insertWith (++) k [rest] acc ) HM.empty headtails in 
                    HashNode (HM.map createHashTree mymap)
                        
                    
  
  
type KeyLength = Int
type WordLength = Int 
crack :: CypherText -> KeyLength -> WordLength ->IO [(PlainText,Key)]
crack c keylen wordlen = do
  realwords <- dicSet wordlen
  let keys = getKeys keylen
  let cypherWord = take wordlen c 
  let x = filter (\(p,_) -> p `S.member` realwords) $ map (decrypt_ cypherWord) keys 
  let x' = map (\(_,key) -> decrypt_ c key) x 
  return x'
  
crackOpt1 :: CypherText -> KeyLength -> WordLength -> IO [(PlainText, Key)]
crackOpt1 c keylen wordlen = do
    cores <- C.getNumProcessors
    hashTree <- dicHashTree wordlen
    let key1 = take keylen $ repeat 'A'
    let cword = take wordlen c 
    keys <- crackOpt1_handler key1 cword hashTree []
    
    let res =  map (decrypt_ c) keys 
    print res 
    return res
crackOpt1_handler :: Key -> CypherText -> HashTree Char Char ->[Key] ->IO [Key] 
crackOpt1_handler curKey c d goodkeys=do
  --putStrLn $ "curKey: " ++ curKey
  res <- crackOpt1_helper c d curKey 0
  case res of 
    Left i -> case incKeyAt i curKey of 
      Nothing     -> return goodkeys
      Just newKey -> do 
        --putStrLn $ "(newKey,I): " ++ (show (newKey,i)) 
        crackOpt1_handler newKey c d goodkeys
    Right _ -> do 
      --putStrLn $ "SUCCESSFUL DECRYPTION WITH: " ++ curKey 
      --_ <- getLine
      case incKeyAt ((length curKey) -1) curKey of 
        Nothing     -> return $ goodkeys ++ [curKey]
        Just newKey -> crackOpt1_handler newKey c d (goodkeys ++ [curKey])
      
incKeyAt :: Int -> Key -> Maybe Key 
incKeyAt i k= let k' = incKeyAtHelper i k in
  if foldr (\ a acc -> acc && (a == 'A')) True k' then
    Nothing
  else 
    return k'
    
incKeyAtHelper :: Int -> Key -> Key
incKeyAtHelper (-1) k = map (\_ -> 'A') k    
incKeyAtHelper i k = let i' = if i >= (length k) then  --just increment the last char of key if the decrypt error happens after one repetition of the key.  
                                (length k) - 1 
                              else 
                                i
                         (keep,die) = splitAt i' k 
                         c = k !! i'
                         c' = encrypt' (c,'B') in -- shift one place. 
                     case c' of 
                       'A' -> incKeyAtHelper (i'-1) k 
                       x@_ -> keep ++ (x: (map (\_ -> 'A' ) (tail die)))
                       
crackOpt1_helper :: CypherText -> HashTree Char Char -> Key -> Int -> IO (Either Int () )
crackOpt1_helper [] _ _ _ = return $ Right () 
crackOpt1_helper (c:c') d (k:k') i =do
  let p = decrypt' (c,k)
  case mylookup p d of 
    Nothing    -> return $ Left i  
    Just hashT -> crackOpt1_helper c' hashT (k' ++ [k]) (i + 1)

    
    
    
    
    
    

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
                     
 

 
(p1,k1,c1) = ("MSOKKJCOSXOEEKDTOSLGFWCMCHSUSGX", 2, 6)     :: (String, Int,Int)
(p2,k2,c2) = ("OOPCULNWFRCFQAQJGPNARMEYUODYOUNRGWORQEPVARCEPBBSCEQYEARAJUYGWWYACYWBPRNEJBMDTEAEYCCFJNENSGWAQRTSJTGXNRQRMDGFEEPHSJRGFCFMACCB", 3, 7) :: (String, Int,Int)
(p3,k3,c3) = ("MTZHZEOQKASVBDOWMWMKMNYIIHVWPEXJA",4,10) :: (String, Int,Int)
(p4,k4,c4) = ("HUETNMIXVTMQWZTQMMZUNZXNSSBLNSJVSJQDLKR",5,11) :: (String, Int,Int)
(p5,k5,c5) = ("LDWMEKPOPSWNOAVBIDHIPCEWAETYRVOAUPSINOVDIEDHCDSELHCCPVHRPOHZUSERSFS", 6, 9) :: (String, Int,Int)
(p6,k6,c6) = ("VVVLZWWPBWHZDKBTXLDCGOTGTGRWAQWZSDHEMXLBELUMO", 7, 13) :: (String, Int,Int)
{-

*BasicVigenere> crack "MSOKKJCOSXOEEKDTOSLGFWCMCHSUSGX" 2 6
[("CAESARSWIFEMUSTBEABOVESUSPICION","KS")]
*BasicVigenere> crack "OOPCULNWFRCFQAQJGPNARMEYUODYOUNRGWORQEPVARCEPBBSCEQYEARAJ
UYGWWYACYWBPRNEJBMDTEAEYCCFJNENSGWAQRTSJTGXNRQRMDGFEEPHSJRGFCFMACCB" 3 7
[("FORTUNEWHICHHASAGREATDEALOFPOWERINOTHERMATTERSBUTESPECIALLYINWARCANBRINGABOUT
GREATCHANGESINASITUATIONTHROUGHVERYSLIGHTFORCES","JAY")]
*BasicVigenere> crack "MTZHZEOQKASVBDOWMWMKMNYIIHVWPEXJA" 4 10
[("EXPERIENCEISTHETEACHEROFALLTHINGS","IWKD")]
*BasicVigenere> crack "HUETNMIXVTMQWZTQMMZUNZXNSSBLNSJVSJQDLKR" 5 11
[("IMAGINATIONISMOREIMPORTANTTHANKNOWLEDGE","ZIENF")]
*BasicVigenere>
*BasicVigenere> crack "LDWMEKPOPSWNOAVBIDHIPCEWAETYRVOAUPSINOVDIEDHCDSELHCCPVHRP
OHZUSERSFS" 6 9
[("EDUCATIONISWHATREMAINSAFTERONEHASFORGOTTENWHATONEHASLEARNEDINSCHOOL","HACKER"
)
-}