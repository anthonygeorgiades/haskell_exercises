import Data.Char

member :: Eq a => a -> [a] -> Bool
member _ [] = False
member item (h:t) = if item == h then True else member item t 
-- Returns true if the element occurs in the list.

count :: (Num a, Eq a1) => a1 -> [a1] -> a
count _ [] = 0
count item list = if item == head list then (+) 1 $ count item $ tail list else count item $ tail list
-- Counts the number of times a value occurs in a list.

forall :: (a -> Bool) -> [a] -> Bool
forall _ [] = False
forall func [item] = func item 
forall func list = if func $ head list then forall func $ tail list else False
--Tests whether all elements of a list satisfy a given condition.

exists :: (a -> Bool) -> [a] -> Bool
exists _ [] = False 
exists func list = if func $ head list then True else exists func $ tail list
----Tests whether any element of a list satisfies a given condition.

single :: (a -> Bool) -> [a] -> Bool
single func list  = sum [1 | x <- list, func x] == 1
--single func list  = sum [1 | x <- list, not func x] == 1
----Tests whether exactly one element of a list satisfies a given condition.

mostly :: (a -> Bool) -> [a] -> Bool
mostly func list  = sum [1 | x <- list, func x] >  div (length list) 2
--function takes 'a' and outputs a boolean- apply function to a list of a's 
--and at some point we output a boolean by applying a to mostly
--True if there are more elements in the list that satisfy the predicate than elements that fail to satisfy the predicate.

isSet :: Eq a => [a] -> Bool
isSet [] = True
isSet (h:t) = if member h t then False else isSet t 
----True if the list contains no duplicate elements.

makeSet :: Eq a => [a] -> [a]
makeSet [] = []
makeSet (h:t) = if member h t then makeSet t else h:makeSet t 
--Remove duplicate elements from a list.

subset :: Eq a => [a] -> [a] -> Bool
subset [] _ = True
subset (h:t) list = if member h list then subset t list else False 
--for a equable- a list with as and another list of as returns true
--True if every element in the first set also occurs in the second set.

equalSet :: Eq a => [a] -> [a] -> Bool
equalSet list1 list2 = if subset list1 list2 && subset list2 list1 then True else False
--True if the two sets contain the same elements (not necessarily in the same order).

setDiff :: Eq a => [a] -> [a] -> [a]
setDiff [] _ = []
setDiff (h:t) list = if member h list then setDiff t list else h:setDiff t list
--The set of values that are in the first set but not in the second set.

setUnion :: Eq a => [a] -> [a] -> [a]
setUnion list1 list2 = makeSet (list1 ++ list2)
--The set of values that are in either or both sets.

setIntersection :: Eq a => [a] -> [a] -> [a]
setIntersection [] _ = []
setIntersection (h:t) list = if member h list then h:setIntersection t list else setIntersection t list
--The set of values that are in both sets.

setBreakByVowel :: [Char] -> [Char]
setBreakByVowel (h:t) = if member h ['a', 'e', 'i', 'o', 'u'] then ' ':h:setBreakByVowel t else h:setBreakByVowel t

wordsSetBreakByVowel :: [Char] -> [[Char]] 
wordsSetBreakByVowel (h:t) = words $ setBreakByVowel(h:t)
--smallProgram :: 

isVowel :: (Char -> Bool) 
isVowel h = member h ['a', 'e', 'i', 'o', 'u', 'A', 'E', 'I', 'O', 'U']

capitals :: [Char] -> Bool
capitals [] = False
capitals (h:t) = isUpper h 
-- capitals (h:[]) = isUpper h

hToLower :: [Char] -> [Char]
hToLower (h:t) = toLower h:t

idigize :: [Char] -> [Char]
idigize list = if list == [] then []
      else 
      let tup = break (isVowel) list  --tuple of list (["nonvowels"], [aasd;]) 
      in if fst tup == [] 
        then let innerTup = span (isVowel) (snd tup) --(["vowels"], ["cascasdcasdc"])
        in if capitals(fst innerTup) then "Idig" ++ hToLower(fst innerTup) ++ idigize (snd innerTup)
        -- in if (isUpper((fst innerTup)!!0)) then "Idig" ++ hToLower(fst innerTup) ++ idigize (snd innerTup)
        else "idig" ++ fst innerTup ++ idigize (snd innerTup)
        -- in "idig" ++ fst innerTup ++ idigize (snd innerTup)
      else
         fst tup ++ idigize (snd tup)

wordFinal :: [Char] -> [Char]
wordFinal sentence = unwords (map idigize (words(sentence)))


assertTrue :: Bool -> String -> IO ()
assertTrue x claim = if x
                       then putStrLn claim
                       else putStrLn $ "*** Untrue: " ++ claim   

assertFalse :: Bool -> String -> IO ()
assertFalse x claim = assertTrue (not x) claim  

assertEqual :: (Eq a, Show a) => a -> a -> IO ()
assertEqual x y = if x == y
                    then putStrLn $ (show x) ++ " equals " ++ (show y)
                    else putStrLn $ "*** " ++ (show x) ++ " does not equal " ++ (show y)

main = do
  assertTrue (member 3 [1, 2, 3, 4]) "3 is in the list [1, 2, 3, 4]"
  assertTrue (member 'a' "hallo") "A is in the word 'hallo' "
  assertTrue (not $ member 7 []) "7 is not in the list []"
  assertFalse (member 7 [1, 2, 3, 4]) "7 is not in the list [1, 2, 3, 4]"
  
  assertEqual 5 (count 'a' "abracadabra")
  assertEqual 4 (count 'a' "bracadabra")
  -- assertEqual 5 (count 'a' "") 
  assertEqual 0 (count 'a' "bbb") 
  
  assertTrue (forall even [2, 4, 6]) "All numbers in the list [2, 4, 6] are even"
  assertFalse (forall even [2, 4, 7]) "All numbers are not even"
  assertTrue (forall (>0) [2, 4, 6]) "All numbers in the list [2, 4, 6] are positive"
  assertFalse (forall (>0) [-2, 4, 6]) "All numbers in the list [-2, 4, 6] are not positive"
  assertFalse (forall (isUpper) ['A', 'b']) "All numbers in the list [-2, 4, 6] are not positive"

  assertTrue (exists even [2, 5, 7]) "There exists an even number in the list [2, 5, 7] "
  assertTrue (exists even [1, 5, 7]) "There does not exist an even number in the list [1, 5, 7] "

  assertTrue (single even [2, 5, 7]) "There is a single even number in the list [2, 5, 7] "
  assertFalse (single even [2, 4, 7]) "There is more than one single even number in the list [2, 4, 7] "
  assertFalse (single even [1, 3, 7]) "There is no single even number in the list [1, 3, 7] "

  assertTrue (mostly even [2, 4, 7]) "There are more even numbers (2) than odd (1) in the list [2, 4, 7] "
  assertFalse (mostly even [2, 3, 7]) "There are not more even numbers (1) than odd (2) in the list [2, 3, 7] "
  assertFalse (mostly even [1, 3, 7]) "There is no single even number in the list [1, 3, 7] "
  assertFalse (mostly >5 [1, 3, 7]) "There are not more numbers greater than 5 than not in the list"
  assertTrue (mostly >0 [1, 3, 7]) "There are mostly more numbers greater than 1 than not in the list"

  assertTrue (isSet [4, 6, 10]) "The set contains no duplicate elements."
  assertTrue (isSet [4, 'a', ""]) "The set contains no duplicate elements of different types as well"
  assertFalse (isSet ['a', 'a', 'b', 4]) "The set contains duplicate string elements"
  assertFalse (isSet [5, 3, 2, 5]) "The set contains duplicate int elements"

  assertEqual [1, 2, 3] (makeSet [1, 1, 2, 2, 3, 3]) "The new set contains no duplicates"
  assertEqual [1, 2, 3, 5] (makeSet [1, 1, 2, 2, 3, 3, 1, 5, 1, 5, 1]) "The new set contains no duplicates"

  assertTrue (subset [1, 2, 3] [1, 2, 3, 4, 5]) "Every element in the first set is in the second"
  assertFalse (subset [1, 2, 3] [2, 3, 4, 5]) "Not every element in the first set is in the second"
  assertTrue (subset [1, 2, 3, 'a', ""] [1, 2, 3, 4, "", 'a', 5]) "Every element in the first set is in the second"
  assertFalse (subset [1, 2, 3, 'a'] [2, 3, 4, 5]) "Not every element in the first set is in the second"

  assertTrue (equalSet [1, 2, 3] [2, 3, 1]) "The sets are equal"
  assertTrue (equalSet [5, 1, 2, 3] [2, 3, 1, 5]) "The sets are equal"
  assertFalse (equalSet [5, 1, 2, 3] [2, 3, 1, 5, 6]) "The sets are not equal"
  assertFalse (equalSet [5, 1, 2, 3] [2, 3, 1, 5, 'a') "The sets are not equal"

  assertEqual [1, 2, 3] (setDiff [1, 2, 3] [5, 6, 7, 8]) "The values are in the first set but are not in the second set"
  assertEqual [1, 2, 'a'] (setDiff [1, 2, 5, 6, 'a'] [5, 6, 7, 8]) "The values are in the first set but are not in the second set"

  assertEqual [1, 2, 3, 4, 5, 6] (setUnion [1, 2, 3] [4, 5, 6]) "The set contains the values of both sets"
  assertEqual [1, 2, 3, 4, 5, 6, 'a'] (setUnion [1, 2, 3] [4, 5, 6, 'a']) "The set contains the values of both sets"

  assertEqual [1, 2, 3, 4, 5, 6] (setIntersection [1, 2, 3, 4, 5, 6, 7, 9] [4, 5, 6, 1, 2, 3, 0, 'a']) "The set contains the set of values in both sets"
  assertEqual [1, 2, 3, 4, 5, 6, 7, 8] (setIntersection [1, 2, 3, 4, 5, 6, 7, 9] [4, 5, 6, 1, 2, 3, 0, 'a', 7, 8]) "The set contains the set of values in both sets"

  assertEqual ['bl', ' ack', ' ack'] (setBreakByVowel ['blackack']) "This is the set broken by vowels"
  assertEqual [' ab', ' ack', ' ack'] (setBreakByVowel ['abackack']) "This is the set broken by vowels"

  assertTrue (isVowel ('a')) "'a' is a vowel"
  assertTrue (isVowel ('e')) "'e' is a vowel"
  assertTrue (isVowel ('i')) "'i' is a vowel"
  assertFalse (isVowel ('x')) "'x' is not a vowel"

  assertTrue (capitals 'H', ['H']) "H is capitalh"
  assertFalse (capitals 'h', ['h']) "h is not capital"

  assertTrue (toLower 'a', ['A']) "a is now lower of A"
  assertTrue (toLower 'b', ['B']) "a is now lower of B"
  assertFalse (toLower 'B', ['B']) "B is NOT lower of B"

  assertEqual ['ridigabbidigit'] (idigize ['rabbit']) "ridigabbidigit is the idigize or rabbit"
  assertEqual ['schidigedidigulidige'] (idigize ['schedule']) "schidigedidigulidige is the idigize or schedule"

