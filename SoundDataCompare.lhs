--------------------------------------------------------------------------------
Author:   Nathan Griffith
Contact:  github.com/smudge
          twitter.com/smudgethefirst
Yale University
CPSC 490 (Senior Project): Spring 2010
--------------------------------------------------------------------------------
SoundDataCompare module:
  Functions for comparing two SoundData sets.
  These functions are useful as fitness functions in a GA.
--------------------------------------------------------------------------------

> module SoundDataCompare where

> import SoundData
> import SoundDataSort

++++++++++++++++++++
Compare Normal Types
++++++++++++++++++++

> --Default comparison sorts by frequency (low to high) and compares partial-by-partial starting at the fundamental
> compareNormal :: SoundData -> SoundData -> Double
> compareNormal a b = helper (sortSoundData a) (sortSoundData b) 0
>   where
>     helper [] b s = s + (sumRemainder b)*100 --the right number of partials is heavily preferred
>     helper a [] s = s + (sumRemainder a)*100 --the right number of partials is heavily preferred
>     helper a b s = helper (tail a) (tail b) (s + (comparePartial (head a) (head b)))
>     comparePartial :: SoundPartial -> SoundPartial -> Double
>     comparePartial a b = (compareFreq a b) + (compareEnv a b)
>     compareFreq a b = abs((pFreq a) - (pFreq b)) --takes the frequency difference
>     sumRemainder xs = foldr (+) 0 (map (\x -> (pFreq x)) xs)
>     compareEnv (SoundPartial _ (LinSeg a1 t1)) (SoundPartial _ (LinSeg a2 t2)) = (compareList a1 a2) + (compareList t1 t2)
>       where 
>         compareList :: [Double] -> [Double] -> Double
>         compareList a b = helper_ a b 0
>         helper_ [] b s = (s + (fromIntegral (length b)))
>         helper_ a [] s = (s + (fromIntegral (length a)))
>         helper_ (a:as) (b:bs) s = helper_ as bs (s+(abs(a-b)))

+++++++++++++++++++++++
Compare Two Basic Types
+++++++++++++++++++++++

> --Default comparison sorts by frequency (low to high) and compares partial-by-partial starting at the fundamental
> compareBasic :: BasicSoundData -> BasicSoundData -> Double
> compareBasic a b = (helper (zip (sortBasic a) (sortBasic b))) + (fromIntegral diff)
>   where
>     helper sbPairs = foldr (+) 0 (map (\(a,b) -> (compareRoot a b)) sbPairs)
>     compareRoot (Sine a b) (Sine c d) = (abs (a - c)) + (abs (b - d)) --basically a distance comparison
>     diff = abs ((length a) - (length b)) --difference in length
