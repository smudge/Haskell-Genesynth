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
> import SoundData


> -- returns a double "a" with a mutation plus or minus "n"
> mutateDouble :: (RandomGen g) => g -> Double -> Double -> (Double, g)
> mutateDouble gen n a = (new_a,new_gen)
>   where out = (randomDouble n gen)
>         new_a = (fst out) + a
>         new_gen = snd out

> -- mutate the frequency of a SoundRoot
> mutateFreqB :: (RandomGen g) => g -> SoundRoot -> (SoundRoot,g)
> mutateFreqB gen (Sine f a) = ((Sine new_f a),new_gen)
>   where out = (mutateDouble gen 5.0 f)
>         new_gen = snd out
>         new_f = fst out
> -- mutate the amplitude of a SoundRoot
> mutateAmplB :: (RandomGen g) => g -> SoundRoot -> (SoundRoot,g)
> mutateAmplB gen (Sine f a) = ((Sine f new_a),new_gen)
>   where out = (mutateDouble gen 0.05 a)
>         new_gen = snd out
>         new_a = (max 0.0 (min 1.0 (fst out)))

> --a list of mutation options for "basic" SoundData
> mutOptionsB :: (RandomGen g) => g -> [SoundRoot -> (SoundRoot,g)]
> mutOptionsB gen = [mutateFreqB gen,mutateAmplB gen]

> --choose a random mutation from a list of mutations ("mutOptionsB")
> chooseMutation gen options = ((array !! get_i),out_gen)
>   where array = (options gen2)
>         out = randomInt 500 gen
>         gen2 = mygen (fst out)
>         new_gen = snd out
>         out2 = randomInt ((length array)-1) new_gen
>         get_i = fst out2
>         out_gen = snd out2

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

> --given a certain probability (0-100) for mutation, return a new BasicSoundData.
>   --IF THE MUTATION is not applied, this simply acts as the identity function.
>   --The algorithm assumes that a mutation may or may not be applied and is indifferent.
> mutateBasic prob options gen sndData
>   | mutationTest = (newSndData,gen_out)
>   | otherwise = (sndData,gen2)
>   where mTestOut = randomInt 99 gen
>         mutationTest = prob > (fst mTestOut)
>         gen2 = snd mTestOut
>         mut_out = chooseMutation gen2 options
>         mutation = fst mut_out
>         gen3 = snd mut_out
>         sd_out = randomInt (length sndData) gen3
>         n = fst sd_out
>         gen_out = snd sd_out
>         newSndData = cycleAndApply mutation sndData n

> --this requires a new generator each time as it does not return one!!!!!
> --use your current generator to randomly generate a seed for this function!!!
> mutateBasicList prob options gen lst = applyToAll (mutateBasic prob options) gen lst

> applyToAll fn gen [] = []
> applyToAll fn gen (a:as) = new_a:(applyToAll fn new_gen as)
>   where out = fn gen a
>         new_a = fst out
>         new_gen = snd out


> testList item = map (\x -> item) [1..5]

mutateNormal

mutateComplex