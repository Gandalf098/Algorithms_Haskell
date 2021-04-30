import Data.Bifunctor

mergeSort :: [Int] -> [Int] -- the main mergeSort func
mergeSort [a] = [a]
mergeSort x = 
    let s = splitSort x
    in mergedSort (fst s, snd s, [])
splitSort :: [Int] -> ([Int], [Int]) -- takes a list and splits it
splitSort x =
  let lsts = splitAt (div (length x) 2) x
   in Data.Bifunctor.bimap mergeSort mergeSort lsts
mergedSort :: ([Int], [Int], [Int]) -> [Int] -- merges the two sorted lists into a single sorted list
mergedSort ([], [], x) = x
mergedSort (a, [], x) = x ++ a
mergedSort ([], b, x) = x ++ b
mergedSort (a:as, b:bs, x) =
    if a < b then mergedSort (as, b:bs, x ++ [a]) else mergedSort (a : as, bs, x ++ [b])


testMS = do
    print $ mergedSort ([], [], [1,2])
    print $ mergedSort ([3], [], [1,2])
    print $ mergedSort ([], [3], [1,2])
    print $ mergedSort ([3], [4], [1,2])

    print $ mergeSort [2,1]
    print $ mergeSort [5,8,1,3,6,9,8,7,13]


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