Genes and Music: A Genetic Programming Algorithm in Haskore
By Nathan Griffith
CS431 Final Project
--------------------------------------------------------------------------------

> module Genes where
> import Haskore
> import System.Random
> import System.IO.Unsafe
> import Data.Maybe
> import Haskore.MidiIO
> import Haskore.UI
> import Data.List(partition)
> import Data.Char(toLower,toUpper)
> import Codec.Midi
> import Test.QuickCheck


Included should be sets of functions, organized by the step of the algorithm.
The algorithm itself should be found at the bottom of the document.

Changes can be made to "favorability" and "mutations" fairly easily, and the
algorithm should respond to and use these changes.

Currently :=: is not supported, but could be added with some adjustments to
the algorithm. (The main focus was getting selection on melodies to work,
which only requires supporting notes connected by :+:)

--------------------------------------------------------------------------------
Test for Interval Favorability:
--------------------------------------------------------------------------------

> totalFav flist = sum (map (\(a,b) -> b) flist)


> intervals :: (Fractional a) => AbsPitch -> Music Pitch -> a -> a -> a
> intervals i (Modify c m) x1 x2 = intervals i m x1 x2
> intervals i (m1@(Primitive _) :+: m2@(Primitive _) :+: x) x1 x2 = 
>                                           intervals i (m2 :+: x) 
>                                                       (x1 + (intervalTest i m1 m2)) 
>                                                       (x2 + 1)
> intervals i (m1@(Primitive _) :+: m2@(Primitive _)) x1 x2 =
>                                         ((x1 + (intervalTest i m1 m2))/(x2 + 1))
> intervals i m x1 x2 = (x1/x2)

> intervalTest :: (Num t) => AbsPitch -> Music Pitch -> Music Pitch -> t
> intervalTest i (Primitive (Note d1 p1)) 
>                (Primitive (Note d2 p2)) = if ((abs ((absPitch p1) - (absPitch p2))) == i)
>                                             then 1
>                                             else 0
> intervalTest i m1 m2 = 0

> intervalFavorability m flist = sum (map (\(a,b) -> (abs ((b/(totalFav flist)) - a))) 
>                                         (map (\(a,b) -> ((intervals a m 0 0),b)) flist)) +
>                                (nonFavoredIntervals m (map (\(x,y) -> x) flist) 0 )

> nonFavoredIntervals (Modify c m) list count = nonFavoredIntervals m list count
> nonFavoredIntervals (n1@(Primitive _) :+: n2@(Primitive _) :+: x) list count = nonFavoredIntervals (n2 :+: x) list (count + (nonFavoredIntervalTest n1 n2 list))
> nonFavoredIntervals (n1@(Primitive _) :+: n2@(Primitive _)) list count = count + (nonFavoredIntervalTest n1 n2 list)
> nonFavoredIntervals m list count = count

> nonFavoredIntervalTest (Primitive (Note d1 p1)) (Primitive (Note d2 p2)) list = if ((abs ((absPitch p1) - (absPitch p2))) `notElem` list)
>                                                  then 1
>                                                  else 0
> nonFavoredIntervalTest n1 n2 l = 0


-----------------------------------------------------------


> notes :: (Fractional a) => AbsPitch -> Music Pitch -> a -> a -> a
> notes ap (Modify c m) x1 x2 = notes ap m x1 x2
> notes ap (n@(Primitive _) :+: x) x1 x2 = notes ap x
>                                                  (x1 + (noteTest ap n)) 
>                                                  (x2 + 1)
> notes ap (n@(Primitive _)) x1 x2 = ((x1 + (noteTest ap n)) / (x2 + 1))
> notes ap m x1 x2 = (x1/x2)

> noteTest :: (Num t) => AbsPitch -> Music Pitch -> t
> noteTest ap (Primitive (Note d p)) = if ((absPitch p) == ap)
>                                     then 1
>                                     else 0
> noteTest ap n = 0

> noteFavorability m flist = sum (map (\(a,b) -> (abs ((b/(totalFav flist)) - a)) ) 
>                                         (map (\(a,b) -> ((notes a m 0 0),b)) flist)) +
>                            (nonFavoredNotes m (map (\(x,y) -> x) flist) 0)

> nonFavoredNotes (Modify c m) list count = nonFavoredNotes m list count
> nonFavoredNotes (n@(Primitive _) :+: x) list count = nonFavoredNotes x list (count + (nonFavoredNoteTest n list))
> nonFavoredNotes (n@(Primitive _)) list count = count + (nonFavoredNoteTest n list)
> nonFavoredNotes n list count = count

> nonFavoredNoteTest (Primitive (Note d p)) list = if ((absPitch p) `notElem` list)
>                                                  then 1
>                                                  else 0
> nonFavoredNoteTest n l = 0


-----------------------------------------------------------


> durationsRun :: (Fractional a) => Dur -> Music Pitch -> a -> a -> a
> durationsRun d (Modify c m) x1 x2 = durationsRun d m x1 x2
> durationsRun d (n@(Primitive _) :+: x) x1 x2 = durationsRun d x
>                                                  (x1 + (durationTest d n)) 
>                                                  (x2 + 1)
> durationsRun d (n@(Primitive _)) x1 x2 = ((x1 + (durationTest d n)) / (x2 + 1))
> durationsRun d m x1 x2 = (x1/x2)

> durationTest :: (Num t) => Dur -> Music Pitch -> t
> durationTest di (Primitive (Note d p)) = if (d == di)
>                                          then 1
>                                          else 0
> durationTest di n = 0

> durationFavorability m flist = sum (map (\(a,b) -> (abs ((b/(totalFav flist)) - a)) ) 
>                                         (map (\(a,b) -> ((durationsRun a m 0 0),b)) flist)) +
>                            (nonFavoredDurations m (map (\(x,y) -> x) flist) 0)

> nonFavoredDurations :: (Fractional a) => Music Pitch -> [Dur] -> a -> a
> nonFavoredDurations (Modify c m) list count = nonFavoredDurations m list count
> nonFavoredDurations (n@(Primitive _) :+: x) list count = nonFavoredDurations x list (count + (nonFavoredDurationTest n list))
> nonFavoredDurations (n@(Primitive _)) list count = count + (nonFavoredDurationTest n list)
> nonFavoredDurations n list count = count

> nonFavoredDurationTest :: (Fractional a) => Music Pitch -> [Dur] -> a
> nonFavoredDurationTest (Primitive (Note d p)) list = if (d `notElem` list)
>                                                      then 1
>                                                      else 0
> nonFavoredDurationTest n l = 0


-----------------------------------------------------------


> beatFavorability m = (nonFavoredBeatRun m 0 0) + (0.1-(beatRun m 0 0 0))

> beatRun (Modify c m) start count total = beatRun m start count total
> beatRun (n@(Primitive _) :+: x) start count total = if (snd (properFraction (start/wn)) == 0)
>                                               then beatRun x (start + (dur n)) (count + 1) (total+1)
>                                               else beatRun x (start + (dur n)) count (total+1)
> beatRun (n@(Primitive _)) start count total = if (snd (properFraction (start/wn)) == 0) 
>                                         then (count + 1)/(total+1)
>                                         else count/(total+1)
> beatRun m start count total = (count/total)

> nonFavoredBeatRun (Modify c m) start count  = nonFavoredBeatRun m start count
> nonFavoredBeatRun (n@(Primitive _) :+: x) start count  = if ((snd (properFraction (start/en)) > 0) && (snd (properFraction (start/sn)) == 0))
>                                               then nonFavoredBeatRun x (start + (dur n)) (count + 1)
>                                               else nonFavoredBeatRun x (start + (dur n)) count
> nonFavoredBeatRun (n@(Primitive _)) start count  = if (snd (properFraction (start/hn)) == 0) 
>                                         then (count + 1)
>                                         else count
> nonFavoredBeatRun m start count  = count

-----------------------------------------------------------
EXAMPLE FAVORABILITIES:

> exampleFavorability = [(0,20),(1,20),(2,40),(3,2),(4,50),(5,15),(7,1),(10,0.1),(12,0.5)]
> tritoneFavored = [(0,10),(1,50),(6,60)]
> minorThirds = [(0,10),(3,50),(5,20)]

> exampleDurationFavorability :: [(Dur,Double)]
> exampleDurationFavorability = [(wn,1),(hn,5),(qn,50),(en,50),(sn,25),(tn,1)]


> evenNoteFavored = (map (\x -> (x,10)) [48..72])

> exampleNoteFavorability = (map (\x -> (x,10 )) [48,50,52,53,55,57,59,62,64,65,67,69,71,72]) ++
>                           (map (\x -> (x,20 )) [60])

 							(map (\x -> (x,0.1 )) [49,51,54,56,58,61,63,66,67,68,70])

Calculate the example favorability of testsong

> iFFJ = intervalFavorability testsong exampleFavorability
> nFFJ = noteFavorability testsong exampleNoteFavorability
> dFFJ = durationFavorability testsong exampleDurationFavorability

Total favorability rating:

> favorability m i n d b = if (b == 1) 
>                          then ((intervalFavorability m i) + (durationFavorability m d) + (noteFavorability m n))/3 + (beatFavorability m)
>                          else ((intervalFavorability m i) + (durationFavorability m d) + (noteFavorability m n))/3

Choose a random element from a list:

> chooseMe :: [a] -> IO a
> chooseMe a = do r <- getStdRandom (randomR (1, length a))
>                 return $ a !! (r-1)

--------------------------------------------------------------------------------
Mutations:
--------------------------------------------------------------------------------

> noteMutations = [mutatePitch,halveNote,doubleNote]

> absIM :: [Int]
> absIM = [1..12]
> intervalMutations :: [Int]
> intervalMutations = absIM ++ (map (\x -> (-x)) absIM)
> getInterval :: IO Int
> getInterval = chooseMe intervalMutations

> mutatePitch :: Music Pitch -> Music Pitch
> mutatePitch (Primitive (Note d p)) = (Primitive (Note d (pitch ((absPitch p) + (unsafePerformIO getInterval)))))
> mutatePitch (Primitive (Rest d)) = (Primitive (Note d (pitch (60 + (unsafePerformIO getInterval)))))
> mutatePitch m = m

> halveNote :: Music Pitch -> Music Pitch
> halveNote (Primitive (Note d p)) = (note (d/2) p)
> halveNote (Primitive (Rest d)) =   (Primitive (Rest (d/2)))
> halveNote m = m

> doubleNote :: Music Pitch -> Music Pitch
> doubleNote (Primitive (Note d p)) = (note (d*2) p)
> doubleNote (Primitive (Rest d)) = (Primitive (Rest (d*2)))
> doubleNote m = m

> chooseNoteMutation = (chooseMe noteMutations)


> shouldMutate chance = (unsafePerformIO (getStdRandom (randomR (1,100)))) < chance

> cycleNoteMutations :: Music Pitch -> Int -> [Music Pitch -> Music Pitch] -> Music Pitch
> cycleNoteMutations m chance noteMuts = if (shouldMutate chance)
>                                        then performMutation m (unsafePerformIO (chooseMe noteMuts))
>                                        else m

> performMutation m mut = cycleMutation m mut (unsafePerformIO (chooseMe [0..(primitiveCount m 0)]))

 performMutation m mut = m

> cycleMutation (n@(Primitive _) :+: x) mut count = if (count == 0)
>                                                    then ((mut n) :+: x)
>                                                    else n :+: (cycleMutation x mut (count-1))
> cycleMutation (n@(Primitive _)) mut count = if (count == 0)
>                                              then (mut n)
>                                              else n
> cycleMutation m mut count = m

> primitiveCount (n@(Primitive _) :+: x) count = primitiveCount x (count+1)
> primitiveCount (n@(Primitive _)) count =  count
> primitiveCount m count = count

Calculate the "example" interval favorability of a mutated song with 30 chance

> intervalMutFav song = intervalFavorability (cycleNoteMutations song 30 noteMutations) exampleFavorability

-------------------------------------------------
SEXUAL REPRODUCTION:
-------------------------------------------------

> produceChildren ms = zipWith produceChild ms (tail ms ++ [head ms])

> produceChild m1 m2 = runChild m1 m2 [0,1] 0

> runChild (n1@(Primitive _) :+: x1) (n2@(Primitive _) :+: x2) chance choice = 
>                        if (choice == 0) 
>                        then (n1 :+: (runChild x1 x2 chance (unsafePerformIO (chooseMe chance))))
>                        else (n2 :+: (runChild x1 x2 chance (unsafePerformIO (chooseMe chance))))
> runChild (n1@(Primitive _)) (n2@(Primitive _)) chance choice = 
>                        if (choice == 0) 
>                        then n1
>                        else n2
> runchild m1 m2 chance choice = if (choice == 0)
>                                then m1
>                                else m2

Parents to test with:

> testparent1 = foldr (:+:) (f 5 qn) (take 19 (iterate (\x -> x) (f 5 qn)))
> testparent2 = foldr (:+:) (c 5 qn) (take 19 (iterate (\x -> x) (c 5 qn)))


--------------------------------------------------------------------------------
The Algorithm:
--------------------------------------------------------------------------------

> push :: [a] -> a -> Int -> [a]
> push as a x =if   length as == x
>                    then a:(init as)
>                    else a:as

> geneticAlgorithm m intervalFav noteFav durFav beatFav mutRate noteMuts generations ifSexual = 
>               runGenerations [m] (favorability m intervalFav noteFav durFav beatFav) intervalFav noteFav durFav beatFav mutRate noteMuts generations ifSexual

> runGenerations ms fms intFav noteFav durFav beatFav mutRate noteMuts 0 ifSexual = head ms
> runGenerations ms fms intFav noteFav durFav beatFav mutRate noteMuts gens ifSexual = 
>               runGenerations (runGeneration ms fms intFav noteFav durFav beatFav mutRate noteMuts ifSexual) fms intFav noteFav durFav beatFav mutRate noteMuts (gens-1) ifSexual

> runGeneration ms fms intFav noteFav durFav beatFav mutRate noteMuts ifSexual = 
>                       (chooseMoreFavored ms fms 
>                          ((map (\m -> 
>                            (cycleNoteMutations m mutRate noteMuts)) ms) ++ (if (ifSexual == 1) then produceChildren ms else [])) intFav noteFav durFav beatFav)

> chooseMoreFavored ms fms [] intFav noteFav durFav beatFav = ms
> chooseMoreFavored ms fms ns intFav noteFav durFav beatFav = 
>       if (favorability (head ns) intFav noteFav durFav beatFav) < fms
>       then chooseMoreFavored (push ms (head ns) 4) (favorability (head ns) intFav noteFav durFav beatFav) (tail ns) intFav noteFav durFav beatFav
>       else chooseMoreFavored ms fms (tail ns) intFav noteFav durFav beatFav

> testsong = foldr (:+:) (cs 5 qn) (take 19 (iterate (\x -> x) (cs 5 qn)))
> rhythm_song = (cs 5 qn) :+: (cs 5 en) :+: (cs 5 en) :+: (cs 5 qn) :+: (cs 5 qn) :+: (cs 5 en) :+: (cs 5 en) :+: (cs 5 en) :+: (cs 5 en) :+: (cs 5 qn) :+: (cs 5 qn)


Here are example runs of the algorithm. Any could be modified, using custom favorability ratings, and custom options.
The usage is as follows:

geneticAlgorithm m intervalFav noteFav durFav beatFav mutRate noteMuts generations ifSexual

m = a starting music value.
intervalFav, noteFav, durFav = favorability ratings for interval, notes, and durations. (See the favorability section for how to write these)
beatFav = 1 if 4/4 downbeats should be favored, 0 if not
mutRate = number (out of 100) which will supply the chance of a single song mutating
noteMuts = a list of mutation functions to be applied on individual notes
generations = the number of generations to run the algorithm for
ifSexual = 1 if sexual reproduction, 0 if asexual

> testAlgorithm = tempo 2 (geneticAlgorithm testsong exampleFavorability exampleNoteFavorability exampleDurationFavorability 1 50 noteMutations 1000 1)
> tritoneAlgorithm = tempo 2 (geneticAlgorithm testsong tritoneFavored evenNoteFavored exampleDurationFavorability 1 50 noteMutations 1000 1)
> minorAlgorithm = tempo 2 (geneticAlgorithm testsong minorThirds evenNoteFavored exampleDurationFavorability 1 50 noteMutations 1000 1)

> runExample = saveMidi "genes.mid" testAlgorithm
> runTritone = saveMidi "genes.mid" tritoneAlgorithm
> runMinor = saveMidi "genes.mid" minorAlgorithm


--------------------------------------------------------------------------------
Save To Midi:
--------------------------------------------------------------------------------

> saveMidi name m = exportFile name (testMidi m)

--------------------------------------------------------------------------------
Thoughts:
--------------------------------------------------------------------------------

Favorability:
-Harmonies?
-Prefer downbeats without excluding more complicated rhythms
-More than just single intervals (chord progressions, etc)

Mutations:
-More mutations
-Adding,subtracting rests
-Split and combine notes
-Switch parts of song

Sexual Reproduction:
-More than just random combination of all notes swapping groups
-Mating choices - mate top sample with all others in population? (i.e. the "alpha" song, "silverback")

The algorithm:
-Allow for larger/smaller population?
-Sexual vs. Asexual. 
-Choosing mutations of less-favorable sections rather than randomly assigning them -- "fixing" or "genetically modifying" (i.e. bacteria in labs)

A Good UI:
-What would be needed?
-How simple/complex?