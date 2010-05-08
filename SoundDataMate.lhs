--------------------------------------------------------------------------------
Author:   Nathan Griffith
Contact:  github.com/smudge
          twitter.com/smudgethefirst
Yale University
CPSC 490 (Senior Project): Spring 2010
--------------------------------------------------------------------------------
SoundDataMate module:
  Functions for mating two SoundData elements
--------------------------------------------------------------------------------

> module SoundDataMate where
> import SoundData
> import SoundDataRandom
> import SoundDataSort
> import System.Random

+++++++++++++++++++++++
Mating of "Basic" types
+++++++++++++++++++++++

> --crossover two basic sound datas
> --this does NOT return a gen, so make sure to send it a new one each time
> --NOTE: depending on the final algorithm, sorting may not be necessary
> crossoverBasic :: (RandomGen g) => BasicSoundData -> BasicSoundData -> g -> BasicSoundData
> crossoverBasic set1 set2 gen = crossoverBasic_ (sortBasic set1) (sortBasic set2) gen
>   where
>     crossoverBasic_ [] set2 gen = set2 --trailing values get tagged on at the end
>     crossoverBasic_ set1 [] gen = set1
>     crossoverBasic_ (a:as) (b:bs) gen = new_item:(crossoverBasic_ as bs new_gen)
>       where bool = randomBool gen
>             new_gen = snd bool
>             new_item | (fst bool) = a
>                      | otherwise  = b

> --crossover a list of BASIC sound datas
> crossoverBasicPop :: (RandomGen g) => [BasicSoundData] -> g -> [BasicSoundData]
> crossoverBasicPop lst gen = concat step1
>   where
>     step1 = map (\x -> (step2 x)) lst
>     step2 x = map (\y -> crossoverBasic x y gen) lst


+++++++++++++++++++++++
Mating of "Normal" types
+++++++++++++++++++++++

> --crossover two normal sound datas
> --this does NOT return a gen, so make sure to send it a new one each time
> --NOTE: depending on the final algorithm, sorting may not be necessary
> crossoverNormal :: (RandomGen g) => SoundData -> SoundData -> g -> SoundData
> crossoverNormal set1 set2 gen = crossoverNormal_ (sortSoundData set1) (sortSoundData set2) gen
>   where
>     crossoverNormal_ [] set2 gen = set2 --trailing values get tagged on at the end
>     crossoverNormal_ set1 [] gen = set1
>     crossoverNormal_ (a:as) (b:bs) gen = new_item:(crossoverNormal_ as bs new_gen)
>       where bool = randomBool gen
>             new_gen = snd bool
>             new_item | (fst bool) = a
>                      | otherwise  = b

> --crossover a list of sound datas
> crossoverNormalPop :: (RandomGen g) => [SoundData] -> g -> [SoundData]
> crossoverNormalPop lst gen = concat step1
>   where
>     step1 = map (\x -> (step2 x)) lst
>     step2 x = map (\y -> crossoverNormal x y gen) lst
