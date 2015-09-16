Indroduction to Cryptography
Homework 1 
Paul Kline


The first thing I tested for when trying to decode problems 1-5 was if any were encrypted simply with a shift cipher. To save time I did not do this check by hand, but rather used some code from our first mini project--namely my encrypt function. The line of Haskell code to 'decrypt' was:

map (\k-> (k,encrypt str k)) (map (:"") ['A'..'Z'])

This first does a Vigenere encrypt using "A" as the key, then "B", etc, effectively checking all possible shifts. Since there aren't that many possibilities, I just looked at each 'decryption' to see if any looked like a real sentence. This revealed the secret messages in problems 1 and 2.

1. 
"WHATSINANAMEAROSEBYANYOTHERNAMEWOULDSMELLASSWEET"
"WHATS IN A NAME? A ROSE BY ANY OTHER NAME WOULD SMELL AS SWEET."
2.
"THEREARETWOTHINGSTOAIMATINLIFEFIRSTTOGETWHATYOUWANTANDAFTERTHATTOENJOYITONLYTHEWISESTOFMANKINDACHIEVETHESECOND"
"THERE ARE TWO THINGS TO AIM AT IN LIFE; FIRST TO GET WHAT YOU WANT, AND AFTER THAT TO ENJOY IT. ONLY THE WISEST OF MANKIND ACHIEVE THE SECOND."

3,4 and 5 did not prove so easy. 

My next strategy to try on 3, 4, and 5 was to see if they were substitution cyphers. So I found some online frequency analysis tools and compared the frequencies from each cipher text to the letter frequencies of the entire English language. Since frequency analysis depends on a large cyphertext, I focused my attention on problem 4, the longest. 

4. jrgdgidxgqanngzgtgttsitgjranmnoeddiomnwjrajvksexjmdxkmnwjrgmttgdtgognjajmzgovgkinlaqgtjamnxmsmjjrgkojtgnwjrgnjrgvattmgtawamnojjrgwizgtnsgnjibabgu 
I found that 'g' had the highest frequency, so I matched it with 'E'. Also import was that the trigraph "jrg" appeared multiple times. I was pretty confident 'g' -> 'E', 'j' -> 'T', and 'r' -> 'H' to give the following partial decryption:
THEdEidxEqannEzEtEttsitETHanmnoeddiomnwTHaTvksexTmdxkmnwTHEmttEdtEoEnTaTmzEovEkinlaqEtTamnxmsmTTHEkoTtEnwTHEnTHEvattmEtawamnoTTHEwizEtnsEnTibabEu

However, I played with this for about an hour and got pretty much nowhere. So being a computer scientist I thought, "hey, I'll write a program that tries all possible remaining mappings and scroll through them and see if anything looks like a sentence." So I did. I also gave each 'decryption' a number that corresponded to how many dictionary words were contained in the decryption, the thought being the higher the better. Now, I understand that the actual, correct solution is only maybe 20 or 30 words at the most. But the logic behind this index I created was that: 
  (1) Many real words contain many other real words as parts, whereas gibberish words contain.. few if any. For example, "that" contains "ha" and "hat". "Maybe" contains "may" and "be". etc. 
  (2) The more real words found in a decryption, the more likely that one of them is an actual word in the actual, proper decryption. Then I can narrow down the remaining mappings even further.  
  
Out of 70,000 decryptions, 67 was the highest index found:
(67,"THEBEIBXEQANNEZESESSRISETHANMNODBBIOMNWTHATVKRDXTMBXKMNWTHEMSSEBSEOENTATMZEOVEKINLAQESTAMNXMRMTTHEKOTSENWTHENTHEVASSMESAWAMNOTTHEWIZESNRENTIGAGEU").

I grew tired of scrolling through 70,000 lines looking for help. Again, as a computer scientist I thought to myself, "wouldn't it be nice if I had a function that could tell me if a decryption was an actual sentence or not?" So I made one--well, sort of. It's a highly recursive function that outputs whether the input string is a real string of words or not. So "applehatsbananashieldfish" will return "True" even though it is not a real sentence; it is a real string of words. To make sure this function works it draws from a slightly modified word dictionary from the one provided for our mini project. Mainly this dictionary includes the words "I" and "A" in it.  Here is an example from a ghci session:

*BasicVigenere Utils> isSentence d "THEREARETWOTHINGSTOAIMATINLIFEFIRSTTOGETWHATYOUWANTANDAFTERTHATTOENJOYITONLYTHEWISESTOFMANKINDACHIEVETHESECOND"
True
*BasicVigenere Utils> isSentence d "THEREARETWOTHINGSTOAIMATINLIFEFIRSTTOGETWHATYOUWANTANDAFTERTHATTOENJOYITONLYTHEWISESTOFMANKINDACHIEVETHESECONDXX"
False

Note that the first input is a proper sentence and returns True and the second sentence is almost identical, but has an extra "XX" at the end. Since "XX" is not a word in our dictionary (and "XX" and no combination of the previous letters make a real word either) "isSentence" returns False (d is dictionary). To make word lookups as fast as possible the 'd' dictionary is a special datatype that is a map of "HashTrees" of characters. Lookups are a function of word length, not of total elements in the set. Therefore, lookups are quite fast: O(1) For one letter words with a max of O(15) for fifteen letter words. Also lookup will "short circuit"; if you are checking to see if "BESTXQZPTBGTT" is a word, only 6 comparisons are performed even though "BESTXQZPTBGTT" is 13 letters. The first is to find the dictionary that contains all 13 letter words, then compares 'B', 'E', 'S', 'T', 'X' and stops since no 13 letter word starts with "BESTX" (in fact, the only 13 letter starting with "best" is "BESTSELLERDOM").

I digress.

I modified the program mentioned above to only printout decriptions that are real sentences. However, after running it for ~20 hours with no output, I killed it. I was expecting as much. The mapping space is rather huge. Checking for real sentences didn't seem helpful. 

I needed to get away from substitution cyphers for a little bit, so I then tried my luck with Vigenere Cyphers. I now had a way to check if a decryption with a key was correct (isSentence). I was able to confirm that problems 3, 4, and 5 were not encrypted with a Vigenere cipher with key lengths up to 4. When a key length of 5 was used, Haskell died. It would run for a while (couple minutes), then print "killed". Usually programs just run forever, but instead Haskell actively gave up. I'll have to ask Dr. Gill about this behaviour. It is related to the internals of list comprehensions and the list monad.  

I went back to trying substitution cyphers. I decided to try to shrink my mapping space. Up to this point, all possible mappings were being considered. For instance, even though 'n' occurs with a frequency of 9.7% in the cyphertext, it still had the possibility to be mapped to 'z', 'j', or 'x' (in fact, all 26 letters), three of the least common letters in the English language. It would be highly improbable that 'n' would map to one of these uncommon characters. So I modified my program a third time to shrink the search space in two ways:
(1) Only try mapping characters to "probable" characters.
(2) Don't bother mapping every single character. 

This time, instead of trying all possible mappings for all characters, it can draw from a list like the following:
mappings = [
             ('j',"T")
           , ('r',"H")
           , ('g',"E")
           , ('d', "LDCRH") 
           , ('i', "AUDLRC")
           , ('x', "CUMWF")
           , ('q', "NPBVKG")
           , ('a', "NIOSH")
           , ('n', "AOINS")
           ]
This is one of my actual lists used in the third iteration of my helper program. The key value pairs are in the form (domain,range). For example, 'd' has a frequency in the cyphertext of 4.1%. I then chose as its range the letters that have a similar distribution in the English language (2.8-6.0%, excluding 'u' because I didn't think it fit). I kept in mind the characters that showed up as pairs multiple times like "tt" (3 times) and excluded mapping 't' to things like "X", "J", and "A" where the double of that letter wouldn't really make sense. Also note that 'j', 'r', and 'g' only have one letter in each of their ranges since I'm pretty sure that's what they are. Also note that this mapping is only partial, and many letters will remain un-decrypted. I was just hoping to get a better understanding of the plaintext and slowly narrow down what it is. In this particular mapping seen above, I was trying to see decryptions of just the first X number of characters of the cyphertext.

However after trying several different mappings in this way and tens of thousands of decryptions to scroll through, I had no further breakthroughs. Lines looked more like English though, which I thought was a pretty good start. Below are a couple interesting decryptions. Especially interesting parts are upperlined:
___                   ___              ____             ____                                                          ___________ 
THEBEIBXEQANNEZESESSRISETHANMNODBBIOMNWTHATVKRDXTMBXKMNWTHEMSSEBSEOENTATMZEOVEKINLAQESTAMNXMRMTTHEKOTSENWTHENTHEVASSMESAWAMNOTTHEWIZESNRENTIGAGEU
___________                                                                                               ________ 
THEDEADMENIOOEzEtEttsAtETHIOmOoeDDAomOwTHITRkseMTmDMkmOwTHEmttEDtEoEOTITmzEoREkAOlINEtTImOMmsmTTHEkoTtEOwTHEOTHERIttmEtIwImOoTTHEwAzEtOsEOTAbIbEu

At this point I started to give up on number 4 and needed to try another strategy. I then went through every single "columnization" for numbers 3, 4, and 5 to see if once written as a matrix, the message would "appear" in the columns. No luck there either. 

I then went back to number 3 with the hopes that it was easier since the number was lower. I knew it wasn't a simple shift cipher so I played around with it awhile and finally  figured it out if you reverse the alphabet and do a shift of 17. Here is an excerpt from a ghci session to aid me as I went: 

*Utils> let mmap3 = [('p','T'),('b','H'),('e','E'),('g','C'),('u','O'),('y','K'),('m','W'),('i','A'),('q','S'),('f','D'),('c','G'),('v','N')]
*Utils> let str3 = "pbeguuymiqicuufguuyiqguuyqcuivfiqguuyqcuqbemevp"
*Utils> replace' mmap3 str3 
"THECOOKWASAGOODCOOKASCOOKSGOANDASCOOKSGOSHEWENT"

3. 
"THE COOK WAS A GOOD COOK AS COOKS GO, AND AS COOKS GO, SHE WENT."