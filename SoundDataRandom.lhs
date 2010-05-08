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

++++++++++++++++++++++++++++
Quick Syntax for mkStdGen
++++++++++++++++++++++++++++

> -- returns a generator given the seed
> mygen :: Int -> StdGen
> mygen seed = mkStdGen (seed :: Int)

++++++++++++++++++++++++++++
Handy-Dandy-Randy Functions
++++++++++++++++++++++++++++
-- quickly generate a random value using one of these functions:

> randomDouble :: (RandomGen g) => Double -> g -> (Double, g)
> randomDouble n gen = randomR ((-n)::Double,n::Double) gen

> randomInt :: (RandomGen g) => Int -> g -> (Int, g)
> randomInt n gen = randomR (0::Int,n::Int) gen

> randomBool :: (RandomGen g) => g -> (Bool, g)
> randomBool gen = random gen