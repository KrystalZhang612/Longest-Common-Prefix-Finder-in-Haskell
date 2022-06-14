
import System.IO
import Text.Printf

--using Eq typeclass to provide an interface for testing for equality

lcp::Eq testing => [[testing]] -> [testing]

lcp [] = []

--use foldr1 to apply to the non-empty strings list given 

lcp stringList = foldr1 mapCommonPrefix stringList

  where
    --use map fst $ takeWhile (uncurry (==)) $ zip length 1 length 2 
    --to pair each element of the list with previous element in the string list
    --take pairs, remove the second element of the pair
    --then stick the first element back on
    
    mapCommonPrefix length1 length2 = map fst.takeWhile (uncurry (==)) $ zip length1 length2

--check if found the proper lcp of all string elements in the given list 

checkIfLcp::[String] -> Bool

checkIfLcp subString =

    let lcpFound = lcp subString in
    
    let theLength = length lcpFound in
    
    --if true, take the substring as the proper LCP found 
    
    all (\stringList -> take theLength stringList == lcpFound ) subString


--main/driver codes 
    
    
main::IO()

main = do
    
    --required testing case 
    
    print $ lcp["apple", "app", "aple", "appl"]
    
    --extra testing cases for debugging 
    
    print $ lcp["anticlimax", "antiaircraft", "antiseptic", "antibody"] 
    
    print $ lcp["circumvent", "circumnavigate", "circumscribe"]
    
    print $ lcp ["hyperactive", "hypersensitive", "hypercritical"]
    
    print $ lcp ["unfinished", "unskilled", "ungraceful", "unfriendly"]

    print $ lcp ["precede", "predict", "project", "prologue"]
    
    print $ lcp ["transmit", "transaction", "translation", "transfer"]
    
    
    