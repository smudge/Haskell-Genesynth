--------------------------------------------------------------------------------
Author:   Nathan Griffith
Contact:  github.com/smudge
          twitter.com/smudgethefirst
Yale University
CPSC 490 (Senior Project): Spring 2010
--------------------------------------------------------------------------------
SoundDataMutate module:
  Functions for sorting a SoundData
--------------------------------------------------------------------------------

> module SoundDataSort where
> import SoundData
> import Data.List


> --Sort function (sorts by frequency, low to high)
> sortSoundData :: SoundData -> SoundData
> sortSoundData = sortBy sortSD
>   where
>     sortSD a b
>       | fa > fb = GT
>       | otherwise = LT
>       where fa = (pFreq a)
>             fb = (pFreq b)


> --Sort pairs of SoundData values and "comparison" values
> sortPairs :: [(BasicSoundData,Double)] -> [(BasicSoundData,Double)]
> sortPairs = sortBy sortSD
>   where
>     sortSD a b
>       | (snd a) > (snd b) = GT
>       | otherwise = LT

> --Sort function (for "Basic" sound data values)
> sortBasic :: BasicSoundData -> BasicSoundData
> sortBasic = sortBy sortSD
>   where
>     sortSD (Sine a _) (Sine b _)
>       | a > b = GT
>       | otherwise = LT