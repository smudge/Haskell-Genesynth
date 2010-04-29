--------------------------------------------------------------------------------
Author:   Nathan Griffith
Contact:  github.com/smudge
          twitter.com/smudgethefirst
Yale University
CPSC 490 (Senior Project): Spring 2010
--------------------------------------------------------------------------------
SoundDataRandom module:
  Useful "random" data generators
--------------------------------------------------------------------------------

> module SoundDataRandom where
> import System.Random

> -- returns a generator given the seed
> mygen :: Int -> StdGen
> mygen seed = mkStdGen (seed :: Int)

> -- quickly generate a random number using one of these functions
> randomDouble n gen = randomR ((-n)::Double,n::Double) gen
> randomInt n gen = randomR (0::Int,n::Int) gen
> randomBool :: (RandomGen g) => g -> (Bool, g)
> randomBool gen = random gen