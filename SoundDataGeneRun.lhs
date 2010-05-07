--------------------------------------------------------------------------------
Author:   Nathan Griffith
Contact:  github.com/smudge
          twitter.com/smudgethefirst
Yale University
CPSC 490 (Senior Project): Spring 2010
--------------------------------------------------------------------------------
SoundDataGeneRun module:
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
> import Data.List (filter)
> import System.Random


+++++++++++++++++++++++++++++++++++++++++++++
Functions For Running and Rendering GA
+++++++++++++++++++++++++++++++++++++++++++++

Very basic functions heavy in defaults:


> runBasic x mut_ops mut_prob crossover_fn start target = runNGens (processPopulation (compareBasic target) crossover_fn (mutateBasic mut_prob mut_ops)) [start] (mygen 20340) x []
> renderBasic name dur timestep sdata = basicRenderComplex name dur timestep sdata
> simpleBasic start target x output = renderBasic output 10.0 0.1 (runBasic x mutOptionsB 100 crossoverBasicPop start target)


> runNormal x mut_ops mut_prob crossover_fn start target = runNGens (processPopulation (compareNormal target) crossover_fn (mutateNormal mut_prob mut_ops)) [start] (mygen 20340) x []

 renderNormal name number_between sdata = normalRenderOrder name number_between sdata
 simpleNormal start target x output = renderNormal output 10 (runNormal x mutOptionsN 100 crossoverNormalPop start target)


+++++++++++++++++++++++
Some Handy Default Data
+++++++++++++++++++++++

> --a list of mutation options for "basic" SoundData
> mutOptionsB :: (RandomGen g) => g -> [BasicPartial -> (BasicPartial,g)]
> mutOptionsB gen = [mutateFreqB gen,mutateAmplB gen,big_mutateFreqB gen]

> --a list of mutation options for "normal" SoundData
> mutOptionsN :: (RandomGen g) => g -> [SoundPartial -> (SoundPartial,g)]
> mutOptionsN gen = [mutateFreqN gen,mutateEnvN gen]

+++++++++++++++++++++
The Genetic Algorithm
+++++++++++++++++++++

> --process 1 iteration on a single population, producing the next generation of individuals
> processPopulation fitness_fn mating_fn mut_fn current_pop randGen = ((new_pop, gen1),top_total_fitness) --returns ((new population, final RandGen), fitness)
>   where
>     rand_out = randomInt 1000 randGen
>     rand_int = fst rand_out
>     gen1 = snd rand_out
>     mutated = applyToAll mut_fn (mygen rand_int) current_pop 
>     mated = mating_fn current_pop (mygen rand_int)
>     new_herd = mutated++mated
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

+++++++++++++++++++++++++++++++++
Parsing And Rendering the Results
+++++++++++++++++++++++++++++++++

> --Convert to a complex SoundData type from the results of a basic genesynth Run
> --This allows one to hear the conversion within a single sound sample
> --Sort of a "morph"/"crossfade" between two BasicSoundData types
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
>         timesList timestep start = timeList_ start
>           where
>             timeList_ start = start : (map ((+) timestep) (timeList_ start))

> --Use the funciton above, but render the results.
> basicRenderComplex name dur timestep basic_data = renderSoundC name (dur*2) (basicRunToComplexData dur timestep basic_data)


+++++++++++++++++++++++++
A Couple Helper Functions
+++++++++++++++++++++++++

> --dealing with triplets
> fst3 (a,b,c) = a
> snd3 (a,b,c) = b
> thd3 (a,b,c) = c
