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
> processPopulation fitness_fn mutProb mutOptions current_pop randGen = ((new_pop, gen1),top_total_fitness) --new population, final RandGen
>   where
>     rand_out = randomInt 1000 randGen
>     rand_int = fst rand_out
>     gen1 = snd rand_out
>     mutated = mutateBasicList mutProb mutOptions (mygen rand_int) current_pop
>     crossed_over = crossoverList current_pop (mygen rand_int)
>     new_herd = mutated++crossed_over
>     fitnessed = map (\x -> (x,fitness_fn x)) new_herd --map fitness function
>     sorted = sortPairs fitnessed
>     de_fitnessed = map fst (sorted)
>     new_pop = take 5 de_fitnessed --populations are limited to 5! 5 5 5 5 5!!!! (this is my magic number -- i could expose it if I wished, but it shouldn't be much more than 5 for speed reasons)
>     top_total_fitness = (snd (head sorted))

> runNGens popRun current_pop randGen 0 history = ((head current_pop):history)
> runNGens popRun current_pop randGen n history 
>   | fitness < 1.0 = ((head current_pop):history) --check for convergence!
>   | otherwise = runNGens popRun new_pop new_gen (n-1) ((head current_pop):history)
>     where process = popRun current_pop randGen
>           new_pop = fst (fst process)
>           new_gen = snd (fst process)
>           fitness = snd process

> testRun x = runNGens (processPopulation (compareBasic hnBasic) 70 mutOptionsB) [otherBasicRepeated] (mygen 123) x []

> testToC = basicRenderComplex "TestComplex.wav" 3.0 0.1 (testRun 1000)

> timesList timestep start = timeList_ start
>   where
>     timeList_ start = start : (map ((+) timestep) (timeList_ start))

> --Convert to a complex SoundData type from the results of a basic genesynth Run
> basicRunToComplexData total_time timestep basic_data = finalData
>   where pairs = reverse (map (map (\(Sine x y) -> (x,y))) basic_data)
>         lst = map (\n -> (pairs !! (round ((fromIntegral n)*((fromIntegral (bd_ln-1))/(fromIntegral (total_num-1))))))) [0..(total_num-1)] --only take total_num
>         bd_ln = length pairs
>         add_times = map (\(as,t) -> (map (\(a,b) -> (a,b,t)) as)) (zip (((head lst):lst)++[(last lst)]) ((0:( take (total_num) (timesList timestep (total_time/2)) ))++[(total_time*2)])) --play the head for a bit
>         partials = map (\n -> col n) [0..(max_length - 1)]
>         max_length = (max (length (head lst)) (length (last lst)))
>         col n = map (\x -> x !! n) add_times
>         fixed_partials = map (\x -> fix_len x) partials
>         fix_len x | (length x) < (total_num+2) = x++[(fst3 (last x),0.0, timestep)] --drop amp
>                   | (length x) > (total_num+2) = take (total_num+2) x --shorten to total number
>                   | otherwise = x --otherwise, yay!
>         total_num | n <= bd_ln = n
>                   | otherwise = bd_ln
>                     where n = (round (total_time/timestep)) --determine the number of partials we need to select
>         makeSoundData as = (ComplexPartial (map (\x -> (thd3 x,fst3 x)) as) (LinSeg (map snd3 as) (map thd3 as)))
>         finalData = map makeSoundData fixed_partials


> --test render a basic run to a single complex sound
> basicRenderComplex name dur timestep basic_data = renderSoundC name (dur*2) (basicRunToComplexData dur timestep basic_data)



> --dealing with tuples
> fst3 (a,b,c) = a
> snd3 (a,b,c) = b
> thd3 (a,b,c) = c

> renderHeadB thing = renderSoundB "renderHead.wav" 2.0 (head thing)

> testPopulation = [mateBasicA,mateBasicA,mateBasicA,mateBasicB,mateBasicB]