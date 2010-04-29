--------------------------------------------------------------------------------
Author:   Nathan Griffith
Contact:  github.com/smudge
          twitter.com/smudgethefirst
Yale University
CPSC 490 (Senior Project): Spring 2010
--------------------------------------------------------------------------------
SoundDataMate module:
  Functions for running a genetic algorithm on SoundData values.
  Input is the starting value, generations are compared to the "final" value.
--------------------------------------------------------------------------------

> module SoundDataGeneRun where
> import SoundData
> import SoundDataRandom
> import SoundDataSort
> import SoundDataMate
> import SoundDataMutate
> import SoundDataCompare
> import SoundDataRender
> import HornTest
> import System.Random

> --process 1 iteration on a single population, producing the next generation of individuals
> processPopulation fitness_fn mutProb mutOptions current_pop randGen = (new_pop, gen1) --new population, final RandGen
>   where
>     rand_out = randomInt 1000 randGen
>     rand_int = fst rand_out
>     gen1 = snd rand_out
>     mutated = mutateBasicList mutProb mutOptions (mygen rand_int) current_pop
>     crossed_over = crossoverList current_pop (mygen rand_int)
>     new_herd = mutated++crossed_over
>     fitnessed = map (\x -> (x,fitness_fn x)) new_herd --map fitness function
>     sorted = map fst (sortPairs fitnessed)
>     new_pop = take 5 sorted

> runNGens popRun current_pop randGen 0 = current_pop
> runNGens popRun current_pop randGen n = runNGens popRun new_pop new_gen (n-1)
>   where process = popRun current_pop randGen
>         new_pop = fst process
>         new_gen = snd process

> testRun2 x = runNGens (processPopulation (compareBasic otherBasic) 70 mutOptionsB) testPopulation (mygen 123) x

> renderHeadB thing = renderSoundB "renderHead.wav" 2.0 (head thing)


> testRun = fst (processPopulation (compareBasic otherBasic) 70 mutOptionsB testPopulation (mygen 123))

> testPopulation = [mateBasicA,mateBasicA,mateBasicA,mateBasicB,mateBasicB]