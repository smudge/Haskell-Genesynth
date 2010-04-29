Nathan Griffith

> module HW1 where
> import Euterpea

> simple :: Integer -> Integer -> Integer -> Integer
> simple x y z = x * (y + z)

 trans :: Int -> Pitch -> Pitch
 trans i p = pitch (absPitch p + i)
 
 
SOLUTIONS:
----------------------

Exercise 1.1 Write out all of the steps in the calculation of the value of
simple (simple 2 3 4) 5 6
=>simple (2 * (3 + 4)) 5 6
=>simple (2 * 7) 5 6
=>simple 14 5 6
=>14 * (5 + 6)
=>14 * 11
=>154

-----------------------------------------------
Exercise 1.2 Prove by calculation that simple (a - b) a b => a^2 - b^2.
(We can tell after one step what the result will be, but I'll show all of the steps anyways)
simple (a - b) a b
=>(a - b) * (a + b)
=>(a - b) * a + (a - b) * b
=>a^2 - ab + ab - b^2
since ab - ab = 0, we end up with:
=>a^2 - b^2

-----------------------------------------------
Exercise 1.3 Identify the well-typed expressions in the following and, for each, give its proper type:
Not Well-typed:
	a = [Cs, 42] :: This is made up of a PitchClass and Integer, but it is not well-typed, since types are being mixed.
	simple ’a’ ’b’ ’c’ :: Assuming simple can only accept Integer values, this is not well-typed. However, if it accepted something like ’a’, it's type might be Char -> Char -> Char -> Char

Well-typed:
	[(2, 3), (4, 5)] :: [(Integer, Integer)]
	(Df ,-42) :: (PitchClass, Integer)
	(simple 1 2 3, simple) :: (Integer, Integer -> Integer -> Integer -> Integer)
	["hello", "world"] :: [String]


-----------------------------------------------
Exercise 2.2 Show that trans i (trans j p) = trans (i + j ) p.

trans i (trans j p)
=> trans i (pitch (absPitch p + j)) 
=> pitch (absPitch (pitch (absPitch p + j))  + i)
( we can see that (pitch ( abspitch (pitch x))) => pitch x -- pitch and abspitch cancel each other out, since they are losless conversions)
=> pitch (absPitch (p + j) + i)
=> pitch (absPitch p + (i + j))

trans (i + j ) p
=> pitch (absPitch p + (i + j))

pitch (absPitch p + (i + j)) == pitch (absPitch p + (i + j))
So, trans i (trans j p) = trans (i + j ) p.


-----------------------------------------------
Exercise 3.2 Rewrite the definition of length non-recursively

length [ ] = 0
length (x : xs) = 1+length xs

> lengthNew :: [a] -> Integer
> lengthNew xs = foldr (+) 0 (map (\x -> 1) xs)


-----------------------------------------------
Exercise 3.3 Define a function that behaves as each of the following:
3. Adds together each pair of numbers in a list. For example:
addEachPair [(1, 2), (3, 4), (5, 6)] => [3, 7, 11]

> addEachPair :: [(Integer, Integer)] -> [Integer]
> addEachPair l = map (\(a, b) -> a + b) l


-----------------------------------------------
Exercise 3.5 Define a function that adds “pointwise” the elements of a list
of pairs. For example:
addPairsPointwise [(1, 2), (3, 4), (5, 6)] => (9, 12)

> addPairsPointwise :: Num a => [(a,a)] -> (a, a)
> addPairsPointwise xs = 
>   let op (a, b) (c, d) = (a + c, b + d)
>   in foldr op (0,0) xs

-----------------------------------------------
*** Exercise 3.6 Define a function chrom::Pitch -> Pitch -> Music Pitch such
that chrom p1 p2 is a chromatic scale of quarter-notes whose first pitch is
p1 and last pitch is p2. If p1 >p2, the scale should be descending, otherwise
it should be ascending. If p1 == p2, then the scale should contain just one
note. (A chromatic scale is one whose successive pitches are separated by
one absolute pitch (i.e. one semitone)).

> chrom :: Pitch -> Pitch -> Music Pitch
> chrom p1 p2
> 	| (absPitch p1) == (absPitch p2) = (Prim (Note qn p2))
> 	| (absPitch p1) > (absPitch p2)  = (Prim (Note qn p1)) :+: (chrom (trans (-1) p1) p2)
> 	| otherwise                      = (Prim (Note qn p1)) :+: (chrom (trans 1 p1) p2)

-----------------------------------------------
*** Exercise 3.7 Abstractly, a scale can be described by the intervals between
successive notes. For example, the 8-note major scale can be defined
as the sequence of 7 intervals [2, 2, 1, 2, 2, 2, 1], and the 12-note chromatic
scale by the 11 intervals [1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1]. Define a function
mkScale :: Pitch -> [Int ] -> Music Pitch such that mkScale p ints is the
scale beginning at pitch p and having the intervallic structure ints.

> mkScale :: Pitch -> [Int] -> Music Pitch
> mkScale p [] = (Prim (Note qn p))
> mkScale p (i : is) = (Prim (Note qn p)) :+: (mkScale (trans i p) is)

-----------------------------------------------
*** Exercise 3.8 Write the melody of “Frere Jacques” (or, “Are You Sleeping”)
in Euterpea. Try to make it as succinct as possible. Then, using functions
already defined, generate a traditional four-part round, i.e. four identical
voices, each delayed successively by two measures. Use a different instrument
to realize each voice.

> frereJacques =
>   let part1 = (c 5 (1/4)) :+: (d 5 (1/4)) :+: (e 5 (1/4)) :+: (c 5 (1/4))
>       part2 = (e 5 (1/4)) :+: (f 5 (1/4)) :+: (g 5 (1/2))
>       part3 = (g 5 (1/8)) :+: (a 5 (1/8)) :+: (g 5 (1/8)) :+: (f 5 (1/8)) :+: (e 5 (1/4)) :+: (c 5 (1/4))
>       part4 = (c 5 (1/4)) :+: (g 4 (1/4)) :+: (c 5 (1/2)) 
>       dub a = a :+: a
>   in tempo 3 (dub part1 :+: dub part2 :+: dub part3 :+: dub part4)

> frereJacquesRound = 
>   let rest3 a = tempo 3 (rest a)
>       frere r instr = ((rest3 r) :+: (instrument instr frereJacques))
>   in  frere 0 DistortionGuitar   :=: 
>      (frere 2 ElectricGuitarJazz :=:
>      (frere 2 FretlessBass       :=: 
>       frere 2 OverdrivenGuitar))


-----------------------------------------------
Exercise 4.2 What is the type of ys in:
xs = [1, 2, 3] :: [Float ]
ys = map (+) xs :: [Float -> Float]

It is type [Float -> Float]

(Because (+) takes two values, and since we are giving it a single Float in each case, it must take another one, and will result in a Float.)


-----------------------------------------------
Exercise 4.3 Define a function applyEach that, given a list of functions,
applies each to some given value. For example:
applyEach [simple 2 2, (+3)] 5 => [14, 8]

> applyEach :: [a -> b] -> a -> [b]
> applyEach [a, b] c = [a c, b c]


-----------------------------------------------
Exercise 4.9 Rewrite this example:
map (\x -> (x + 1) / 2) xs
using a composition of sections

> comp xs = ((/2) . (+1)) xs
