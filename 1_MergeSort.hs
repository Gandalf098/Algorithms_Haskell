import Data.Bifunctor ( Bifunctor(bimap) )

mergeSort :: [Int] -> [Int] -- the main mergeSort func
mergeSort [a] = [a]
mergeSort x =
    let s = splitSort x
    in uncurry mergedSort s

splitSort :: [Int] -> ([Int], [Int]) -- takes a list and splits it, returning two sorted arrays
splitSort x =
  let lsts = splitAt (div (length x) 2) x
   in Data.Bifunctor.bimap mergeSort mergeSort lsts
mergedSort :: [Int] -> [Int] -> [Int] -- merges the two sorted lists into a single sorted list
mergedSort [] [] = []
mergedSort a [] = a
mergedSort [] b = b
mergedSort (a:as) (b:bs) =
    if a < b then a:mergedSort as (b:bs) else b:mergedSort (a:as) bs


testMS = do
    print $ mergedSort [] []
    print $ mergedSort [3] []
    print $ mergedSort [] [3]
    print $ mergedSort [3] [4]

    print $ mergeSort [2,1]
    print $ mergeSort [5,8,1,3,6,9,8,7,13]
    print $ mergeSort [2,3,1]


main = testMS

--Haskell Lessons Learned

-- Easter Egg !, we found a BiFunctor in our CODE !!!!

-- splitSort :: [Int] -> ( [Int], [Int] ) -- takes a list and splits it
-- splitSort x = let lsts = splitAt (div (length x) 2 ) x
--                   in ( mergeSort (fst lsts), mergeSort (snd lsts) ) <----

-- splitSort :: [Int] -> ( [Int], [Int] ) -- takes a list and splits it
-- splitSort x = let lsts = splitAt (div (length x) 2 ) x
--                   in Data.Bifunctor.bimap mergeSort mergeSort lsts <----


-- tried to use no branching to keep it branchless funcitonal code (of course pattern matching is branching under the hood, and basically everything are jumps in assembly, however humans prefer not to use branching especially goto statements)
-- Note not branchless performance wise
-- only a single if is used in mergeSort

-- `div` vs /, integer division vs fractional division

-- splitAt function

--cat :: ( [Int], [Int] ) -> [Int] -- appends two lists together
--cat = uncurry (++)

--cat x == (fst x) ++ (snd x)
--cat x == (++) (fst x) (snd x)
--cat x == uncurry (++) x
--cat == uncurrt (++)

--and a lot more !