import Data.List.Ordered ( foldt )

-- #TODO# Create new monadic type, that acts as a list, while having state variable representing sofar counted inversions
-- we want to make a new type, that is an orderable list, and a hidden value (the inversions from the original list)
-- Kleisli Writer Category fits this description (at least as far as my knowledge can do)
-- 


-- #TODO# wrap old functions (countInversions, inversionCount) into wrappers, and make the original functions be the same as their original copies, 
-- to show how much work was done in total (the fmap, fish, new type ...etc) and how much work per wrapper is needed to make old code do new tricks
-- a.k.a to show how much flexibility is in the old code (for SDF)

type Accumlator a = (a, Int) -- Accumlator is a type, constructor that "Hides" and associated Integer
(+) :: Int -> Accumlator a -> Accumlator a
(+) a b =
    let (x, s1) = b
    in (x, (Prelude.+) s1  a)


-- fish operator, composer
(>=>) :: (a -> Accumlator b) -> (b -> Accumlator c) -> (a -> Accumlator c)
m1 >=> m2 = \x ->
    let (y, s1) = m1 x
        (z, s2) = m2 y
    in (z, s1 Prelude.+ s2)

return :: a -> Accumlator a
return x = (x, 0)

fmap f = id >=> (Main.return . f)

f :: Ord a => Accumlator [a] -> Accumlator [a] -> Accumlator [a]
f a b =
    let (y, s1) = a
        (z, s2) = b
        (r, s3) = countInversions y z
    in (r, s1 Prelude.+ s2 Prelude.+ s3)

countInversions :: Ord a => [a] -> [a] -> Accumlator [a]-- merges the two sorted lists into a single sorted list along with inversions
countInversions [] [] = ([], 0)
countInversions a [] = (a, 0)
countInversions [] b = (b, 0)
countInversions (a:as) (b:bs) =
    if a <= b then Main.fmap (a :) ( countInversions as (b:bs) ) else Main.fmap (b :) ( length (a:as) Main.+ countInversions (a:as) bs )

--fmap f = id >=> (Main.return . f)

inversionCount    :: (Ord a) => [a] -> Int
inversionCount xs = snd (foldt f (Main.return []) ( Prelude.fmap (Main.return . (: [])) xs ))

testMS = do
    print 1
    print $ countInversions [3] []
    print $ countInversions [] [3]
    print $ countInversions [3] [4]

    print $ countInversions [4] [3]

    print $ countInversions [4,6,7] [1,2,5]
    print $ countInversions [4] [4]

    print $ countInversions [3, 4] [1, 2]

    print $ inversionCount [2,1]
    print $ inversionCount [3,4]
    print $ inversionCount [4,3]

    print $ inversionCount [2,1]
    print $ inversionCount [3,4,1,2]

    print $ inversionCount [8, 4, 2, 1]

    print $ countInversions [2] [1]
    print $ inversionCount [2, 1]
    print $ countInversions [8] [4]
    print $ inversionCount [8, 4]
    print $ countInversions [4, 8] [1, 2]
    print $ inversionCount [4, 8, 1, 2]

    print $ inversionCount [3,1,2]

    print $ inversionCount [5,8,1,3,6,9,8,7,13]


main = testMS


-- Haskell Lessons Learned
-- Functors & Monads are great tool in functional languages (Functional Programming)
-- Note: we didn't use functors (except for some ugly fmaps) or Monads here
-- without proper and correct Monads, code would be ugly (as you can see above)
-- and Monads exposes the Mathematical foundations of programming (MFP, or MFS (MF of Software))
--      Since functional programming exposes MFP, however, some (many) [seemingly] small things in Imperative languages (see note1 below)
--      have very complex [side-]effects on the correctness of the mathematical reasoning envolved, and therefore needs meta information
--      these meta information are 'discover' by Programmers as they read through code that isn't their code, through things like Programming Style and Common-Sense (whatever that really means)
--      
--      Note1 : well, here we could edit Merge Sort to just work, using a global variable (or maybe a private variable) for countInversion (which could be thought of as State Variable)
--      it isn't that direct (when denotational semantics is enforced), therefore a structure of definitions and standard interfaces is needed (to make old code "the Merge Sort" be flexible and editable to provide more "functionality" (here the inversion count))
--      Like fmaps and so on
--
--      Note2: when a monad is at work (instead of just a new type like in here) all of this is done under the hood 
--      (interfaces are autmatically made using the fish or monad operator, and therefore only the monad and its functions 
--      'plus' the new needed functionality is implemented (an almost 100% code reusability/ flexibility))
