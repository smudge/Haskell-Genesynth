--------------------------------------------------------------------------------
Author:   Nathan Griffith
Contact:  github.com/smudge
          twitter.com/smudgethefirst
Yale University
CPSC 490 (Senior Project): Spring 2010
--------------------------------------------------------------------------------
SoundDataCompare module:
  Functions for comparing two SoundData sets
--------------------------------------------------------------------------------

> module SoundDataCompare where

> import SoundData
> import SoundDataSort

> --Default comparison sorts by frequency (low to high) and compares partial-by-partial starting at the fundamental
> compareSoundData :: SoundData -> SoundData -> Double
> compareSoundData a b = helper (sortSoundData a) (sortSoundData b) 0
>   where
>     helper [] b s = s + sumRemainder b
>     helper a [] s = s + sumRemainder a
>     helper a b s = helper (tail a) (tail b) (s + (comparePartial (head a) (head b)))
>     comparePartial :: SoundPartial -> SoundPartial -> Double
>     comparePartial a b = (compareFreq a b) + (compareEnv a b)
>     compareFreq a b = abs((pFreq a) - (pFreq b))
>     compareEnv a b = 0
>     sumRemainder xs = foldr (+) 0 (map (\x -> (pFreq x)) xs)

> compareBasic :: BasicSoundData -> BasicSoundData -> Double
> compareBasic a b = (helper (zip (sortBasic a) (sortBasic b))) + (fromIntegral diff)
>   where
>     helper sbPairs = foldr (+) 0 (map (\(a,b) -> (compareRoot a b)) sbPairs)
>     compareRoot (Sine a b) (Sine c d) = (abs (a - c)) + (abs (b - d))
>     diff = abs ((length a) - (length b))
