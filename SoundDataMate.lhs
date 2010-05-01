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

> --crossover two basic sound datas
> --this does NOT return a gen, so make sure to send it a new one each time
> --NOTE: depending on the final algorithm, sorting may not be necessary
> crossoverBasic set1 set2 gen = crossoverBasic_ (sortBasic set1) (sortBasic set2) gen
>   where
>     crossoverBasic_ [] set2 gen = set2 --trailing values get tagged on at the end
>     crossoverBasic_ set1 [] gen = set1
>     crossoverBasic_ (a:as) (b:bs) gen = new_item:(crossoverBasic as bs new_gen)
>       where bool = randomBool gen
>             new_gen = snd bool
>             new_item | (fst bool) = a
>                      | otherwise  = b

> --TODO: update the generator for these s/t each crossover has a new generator
> crossoverList lst gen = concat step1
>   where
>     step1 = map (\x -> (step2 x)) lst
>     step2 x = map (\y -> crossoverBasic x y gen) lst

> --TEST VALUES; to make sure the mating is working
> mateBasicA = [(Sine 100.1 0.1),(Sine 200.1 0.1),(Sine 300.1 0.1),(Sine 400.1 0.1),(Sine 500.1 0.1),(Sine 600.1 0.1)]
> mateBasicB = [(Sine 100.2 0.2),(Sine 200.2 0.2),(Sine 300.2 0.2),(Sine 400.2 0.2),(Sine 500.2 0.2),(Sine 600.2 0.2)]
