--------------------------------------------------------------------------------
Author:   Nathan Griffith
Contact:  github.com/smudge
          twitter.com/smudgethefirst
Yale University
CPSC 490 (Senior Project): Spring 2010
--------------------------------------------------------------------------------
SoundDataMutate module:
  Functions for mutating a SoundData
--------------------------------------------------------------------------------

> module SoundDataMutate where
> import System.Random
> import SoundDataRandom
> import SoundDataSort
> import SoundData
> import List (delete)


+++++++++++++++++++++++
Mutate SoundRoots (or BasicPartial)
+++++++++++++++++++++++

> -- mutate the frequency of a SoundRoot (very small leaps)
> mutateFreqB :: (RandomGen g) => g -> SoundRoot -> (SoundRoot,g)
> mutateFreqB gen (Sine f a) = ((Sine new_f a),new_gen)
>   where out = (mutateDouble gen 1.0 f)
>         new_gen = snd out
>         new_f = fst out
> -- mutate the frequency of a SoundRoot (bigger leaps!)
> big_mutateFreqB :: (RandomGen g) => g -> SoundRoot -> (SoundRoot,g)
> big_mutateFreqB gen (Sine f a) = ((Sine new_f a),new_gen)
>   where out = (mutateDouble gen 100.0 f)
>         new_gen = snd out
>         new_f = fst out
> -- mutate the amplitude of a SoundRoot
> mutateAmplB :: (RandomGen g) => g -> SoundRoot -> (SoundRoot,g)
> mutateAmplB gen (Sine f a) = ((Sine f new_a),new_gen)
>   where out = (mutateDouble gen 0.05 a)
>         new_gen = snd out
>         new_a = (min 1.0 (fst out))



+++++++++++++++++++++++
Mutate Envelope
+++++++++++++++++++++++

> mutateEnv gen (LinSeg as ts)
>   | (fst choice) < 1 = (LinSeg (addA as) (addT ts))
>   | otherwise = (LinSeg new_as new_ts)
>     where choice = randomInt 8 gen --chances are it will modify the values
>           a_index = randomInt (length as -1) (snd choice)
>           t_index = randomInt (length ts -1) (snd (a_index))
>           new_as = modifyA
>           new_ts = modifyT
>           modifyA = cycleAndApply (mutateDouble (snd t_index) 0.05) as (fst a_index)
>           modifyT = cycleAndApply (mutateDouble (snd t_index) 0.05) ts (fst t_index)
>           addA as = as
>           addT ts = ts

+++++++++++++++++++++++
Mutate SoundPartial
+++++++++++++++++++++++


+++++++++++++++++++++++
Choose Mutation From A List
+++++++++++++++++++++++

> --choose a random mutation from a list of mutations ("mutOptionsB")
> chooseMutation gen options = ((array !! get_i),out_gen)
>   where array = (options gen2)
>         out = randomInt 500 gen
>         gen2 = mygen (fst out)
>         new_gen = snd out
>         out2 = randomInt ((length array)-1) new_gen
>         get_i = fst out2
>         out_gen = snd out2

+++++++++++++++++++++++
Mutate BasicSoundData
+++++++++++++++++++++++

> --given a certain probability (0-100) for mutation, return a new BasicSoundData.
>   --IF THE MUTATION is not applied, this simply acts as the identity function.
>   --The algorithm assumes that a mutation may or may not be applied and is indifferent.
> mutateBasic :: (RandomGen g) => Int -> (StdGen -> [BasicPartial -> (BasicPartial, g)]) -> g -> [BasicPartial] -> ([BasicPartial],g)
> mutateBasic prob options gen sndData
>   | mutationTest = (newSndData,gen_out)
>   | otherwise = (sndData,gen2)
>   where mTestOut = randomInt 99 gen
>         mutationTest = prob > (fst mTestOut)
>         gen2 = snd mTestOut
>         mut_out = chooseMutation gen2 options
>         sd_out = randomInt (length sndData - 1) (snd mut_out)
>         n = fst sd_out
>         prob_out = randomInt 3 (snd sd_out)
>         prob2 = fst prob_out
>         gen_out = snd prob_out
>         newSndData
>           | prob2 <= 1 = cycleAndApply (fst mut_out) sndData n
>           | prob2 == 2 = remove_partial n --ADD OR REMOVE HERE
>           | otherwise = add_partial
>             where
>               remove_partial i = delete (sndData !! i) sndData
>               add_partial = sortBasic (sndData ++ [(Sine (10000.0+(fst nf_out)) (0.5+(fst na_out)))])
>               nf_out = (randomDouble 10000 gen2) --random frequency from 0-20000 Hz
>               na_out = (randomDouble 0.5 (snd nf_out)) --random amplitude from 0-1


+++++++++++++++++++++++
Mutate Normal SoundData
+++++++++++++++++++++++

> --given a certain probability (0-100) for mutation, return a new BasicSoundData.
>   --IF THE MUTATION is not applied, this simply acts as the identity function.
>   --The algorithm assumes that a mutation may or may not be applied and is indifferent.
> mutateNormal :: (RandomGen g) => Int -> (StdGen -> [SoundPartial -> (SoundPartial, g)]) -> g -> [SoundPartial] -> ([SoundPartial],g)
> mutateNormal prob options gen sndData
>   | mutationTest = (newSndData,gen_out)
>   | otherwise = (sndData,gen2)
>   where mTestOut = randomInt 99 gen
>         mutationTest = prob > (fst mTestOut)
>         gen2 = snd mTestOut
>         mut_out = chooseMutation gen2 options
>         sd_out = randomInt (length sndData - 1) (snd mut_out)
>         n = fst sd_out
>         prob_out = randomInt 3 (snd sd_out)
>         prob2 = fst prob_out
>         gen_out = snd prob_out
>         newSndData
>           | prob2 <= 1 = cycleAndApply (fst mut_out) sndData n
>           | prob2 == 2 = remove_partial n --ADD OR REMOVE HERE
>           | otherwise = add_partial
>             where
>               remove_partial i = delete (sndData !! i) sndData
>               add_partial = sortSoundData (sndData ++ [(head sndData)])


+++++++++++++++++++++++
Helper Functions
+++++++++++++++++++++++

> --this is used by the GA to apply a mutation to all in a list
> applyToAll fn gen [] = []
> applyToAll fn gen (a:as) = new_a:(applyToAll fn new_gen as)
>   where out = fn gen a
>         new_a = fst out
>         new_gen = snd out

> -- returns a double "a" with a mutation plus or minus "n"
> mutateDouble :: (RandomGen g) => g -> Double -> Double -> (Double, g)
> mutateDouble gen n a = (new_a,new_gen)
>   where out = (randomDouble n gen)
>         new_a = (max 0.0 ((fst out) + a))
>         new_gen = snd out

> -- apply function fn to element n in list lst
> cycleAndApply :: (Num n) => (a -> (a,b)) -> [a] -> n -> [a]
> cycleAndApply fn lst n = helper fn lst n 0
>   where 
>     helper fn [] n1 n2 = []
>     helper fn lst n1 n2
>       | (n1 == n2) = new_item:(tail lst)
>       | otherwise = (head lst):(helper fn (tail lst) n1 (n2+1))
>         where
>           out = (fn (head lst))
>           new_item = fst out
>           new_gen = snd out