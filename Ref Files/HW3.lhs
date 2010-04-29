------------------------------------------------------------------------------------------------
Nathan Griffith
Homework 3
CS 432
October 14, 2009
------------------------------------------------------------------------------------------------

> {-# LANGUAGE Arrows #-}

> import Euterpea hiding (line)
> import Euterpea.Audio.CSoundGenerators
> import Euterpea.Audio.Types
> import Euterpea.Audio.IO
> import Control.Arrow ((>>>), arr, (&&&))

------------------------------------------------------------------------------------------------
Declare some useful types and functions:
------------------------------------------------------------------------------------------------

> type AudSF a b = Signal AudRate a b
> type CtrSF a b = Signal CtrRate a b

> out :: AudSF () Double -> IO ()
> out = outFile "test.wav" 3.0

> f1 = gen10 4096 [1]

> f2 = gen10 4096 [1, 0.5, 0.333, 0.25, 0.2, 0.166, 0.142, 0.125, 
>                  0.111, 0.1, 0.09, 0.083, 0.076, 0.071, 0.066, 0.062]

> instr101 :: AudSF () Double
> instr101 = proc _ -> oscil f1 0 -< 440

> oscilSimple :: Double -> Double -> AudSF () Double
> oscilSimple f a = proc _ -> do 
>                     result <- oscils f -< 1
>                     returnA -< result * a

> constA :: Clock p => a -> Signal p () a
> constA = arr.const

------------------------------------------------------------------------------------------------
Start of problems:
------------------------------------------------------------------------------------------------

++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
1. Copy the file Euterpea/example/CSoundExamples.lhs into a file HW3.lhs, somewhere where you 
can modify and experiment with it.  Run GHCi, load the new file, and experiment with the 
various examples.  As you run each example, listen to and look at the results (in the 
test.wav file) using Audacity.
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

:-) this was fun! (copied many of them to the "useful functions" above)

++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
2. Implement in Euterpea the signals implied in Figures 1.17 and 1.18 of Chapter 1 in TcB, 
and listen to the results.  What do you hear?
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

> -----------------------------------------------
> f1osc :: AudSF () Double
> f1osc = proc _ -> do
>            oscil f1 0 -< 1000
> f1asig :: IO ()
> f1asig = outFile "f1asig.wav" 3.0 f1osc
> -----------------------------------------------

> -----------------------------------------------
> f1osc' :: CtrSF () Double
> f1osc' = proc _ -> do
>            oscil f1 0 -< 1000
> f1ksig :: IO ()
> f1ksig = outFile "f1ksig.wav" 3.0 f1osc'
> -----------------------------------------------

f1asig -> f1ksig seems to transpose the pitch slightly
due to aliasing or something...

> -----------------------------------------------
> f2osc :: AudSF () Double
> f2osc = proc _ -> do
>            oscil f2 0 -< 1000
> f2asig :: IO ()
> f2asig = outFile "f2asig.wav" 3.0 f2osc
> -----------------------------------------------


> -----------------------------------------------
> f2osc' :: CtrSF () Double
> f2osc' = proc _ -> do
>            oscil f2 0 -< 1000
> f2ksig :: IO ()
> f2ksig = outFile "f2ksig.wav" 3.0 f2osc'
> -----------------------------------------------

f2asig -> f2ksig seems to have transposed it down a lot due to aliasing... 
it sounds like a much lower note.

++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
3. Define a function:
    tremolo :: Double -> Double -> AudSF Double Double
such that "tremolo f d" is a signal function that adds tremolo to a signal, where f is the 
tremolo frequency, and d is the tremolo depth.
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

> tremolo :: Double -> Double -> AudSF Double Double
> tremolo f d = proc a -> do
>                 mod <- oscil f1 0 -< f
>                 returnA -<  (((1 + d * mod)) * a) / (1 + d) --include some normalization here too

=================
Test/Use Examples
=================

> test_tremolo :: IO ()
> test_tremolo = outFile "tremolo.wav" 3.0 (instr101 >>> tremolo 5 0.8)

Note: I've written this such that 0 <= d <= 1. When d > 1 or < 0 the value doesn't really make any sense.\
I also did some normalization in the tremolo function so that the result is never louder than the original
wave (to prevent clipping).

++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
4. Define a function:
    vibrato :: Double -> Double -> AudSF Double Double
such that "vibrato f d" is a signal function that takes a frequency argument (be careful -- 
this is not a signal of a given frequency, it is the frequency itself), and generates a 
signal at that frequency, but with vibrato added, where f is the vibrato frequency, and 
d is the vibrato depth.
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

> vibrato :: Double -> Double -> AudSF Double Double
> vibrato f d = 
>     proc freq -> do
>       mod <- oscil f1 0 -< f
>       oscil f1 0 -< (freq + d * mod) --shouldn't need normalization

=================
Test/Use Examples
=================

> test_vibrato :: IO ()
> test_vibrato = outFile "vibrato.wav" 3.0 (constA 440 >>> vibrato 10 5)

++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
5. Change the definition of either "tremolo" or "vibrato" so that the frequency and depth 
arguments are both signals themselves, so that we can vary them dynamically.  (I.e. you 
will need a function whose type is something like AudSF (Double,Double,Double) Double.)
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

> vibrato' :: AudSF (Double,Double, Double) Double
> vibrato' = proc (freq,f,d) -> do
>              mod <- oscil f1 0 -< f
>              result <- oscil f1 0 -< (freq + d * mod)
>              returnA -<  result --shouldn't need normalization

=================
Test/Use Examples
=================

Let me write a new operator to help me route 3 signals into my new function:

> (>>>>) :: Clock p => (Signal p a b, Signal p a c, Signal p a d) -> Signal p (b,c,d) f -> Signal p a f
> (sf1, sf2, sf3) >>>> sf4 = 
>     proc a -> do
>       b <- sf1 -< a
>       c <- sf2 -< a
>       d <- sf3 -< a
>       f <- sf4 -< (b,c,d)
>       returnA  -< f

> test_vibrato' :: IO ()
> test_vibrato'2 :: IO ()
> test_vibrato'  = outFile "vibrato'.wav" 3.0 ((constA 440, (constA 440 >>> line 0 100 3), constA 10) >>>> vibrato')
> test_vibrato'2 = outFile "vibrato'2.wav" 3.0 ((constA 440, constA 7, (constA 440 >>> line 0 200 3)) >>>> vibrato')

++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
6. Define a function:
    adsr :: DPair -> DPair -> DPair -> DPair -> AudSF Double Double
    type DPair = (Double, Double)
such that "adsr (attD,attL) (decD,decL) (susD,susL) (relD,relL)" is a signal function that 
"envelopes" its signal argument with an ADSR envelope whose attack, decay, sustain, and 
release durations are given by the "D" arguments, and whose corresponding levels are given 
by the "L" arguments.
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

> type DPair = (Double, Double)

> adsr :: DPair -> DPair -> DPair -> DPair -> AudSF Double Double
> adsr (attD,attL) (decD,decL) (susD,susL) (relD,relL) = 
>      proc a -> do
>      f <- linseg [0, attL, decL, susL, relL, 0, 0] [attD, decD, susD, relD, 0, 0] -< ()
>      returnA -< f * a

=================
Test/Use Examples
=================

> test_adsr :: IO ()
> test_adsr = outFile "adsr.wav" 8.0 (instr101 >>> adsr (2,0.1) (1,0.5) (3,1) (2, 0))

++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
7. Generalize instr120 in the following way:  Define a function:
    chorus :: Int -> Double -> AudSF Double Double
such that "chorus n x" is a signal function that takes a frequency argument (be careful -- 
this is not a signal of a given frequency, it is the frequency itself), and generates a signal 
at that frequency (say f), plus signals at f ± jx, for 1 <= j <= n.  For example, 
"chorus 1 0.01 -< ifrq" would achieve the effect in Figure 1.33.
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

> chorus :: Int -> Double -> AudSF Double Double
> chorus n x = 
>     proc a -> do 
>         out <- chorus_ n x -< a
>         returnA -< out / (2 * (fromIntegral n) + 1) --normalize the output
>     where 
>       chorus_ 0 x = oscil f1 0
>       chorus_ n x =
>           proc a -> do 
>               b <- oscil f1 0 -< a + x * (fromIntegral n)
>               c <- oscil f1 0 -< a - x * (fromIntegral n)
>               d <- chorus_ (n-1) x -< a
>               returnA -< c + b + d

=================
Test/Use Examples
=================

> test_chorus :: IO ()
> test_chorus_wide :: IO ()
> test_chorus = outFile "chorus.wav" 3.0 (constA 440 >>> chorus 1 1)
> test_chorus_wide = outFile "chorus_wide.wav" 3.0 (constA 1000 >>> chorus 4 200)


++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
8. Change the function instr136 in CSoundExamples so that it avoids the five calls to vdelay, 
and instead calls a function (say foo) that takes 5 as an argument (plus perhaps some other 
things), and uses recursion to implement the 5 vdelays.
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

> instr136 :: Double -> Double -> Double -> Double -> Double -> Double -> AudSF () Double
> instr136 dur amp frq rat1 rat2 maxdel =
>     proc _ -> do
>       rte <- line rat1 dur rat2 -< 1
>       lfo0 <- oscil f1 0 -< rte / dur
>       let lfo = (lfo0 + 1) / 2 * maxdel
>       noise <- pluck f1 frq SimpleAveraging -< frq
>       mix <- instr136_ 4 -<  (noise*amp, lfo)
>       linen 0.01 dur 0.1 -< mix
>     where
>       instr136_ :: Int -> AudSF (Double, Double) Double
>       instr136_ 0 = vdelay maxdel
>       instr136_ n = proc (del, lfo) -> do
>                    del <- vdelay maxdel -< (del, lfo)
>                    rest <- instr136_ (n-1) -< (del, lfo)
>                    returnA -< del + rest

=================
Test/Use Examples
=================

> test_instr136 :: IO ()
> test_instr136 = outFile "instr136.wav" 3.0 (instr136 3.0 0.1 500 2 0.6 0.01)

++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
9. Design an amplitude modulator that realizes the equation:
    cos(C) * (offset + a*cos(M))
where C, M, a, and offset are all variable.  This means we want a function whose type is:
    am :: AudSF (Double,Double,Double,Double) Double
Play with this.  Do some wild and crazy things.  Sweep (using envelopes) some of the inputs.  
Stack a few AM modulators together.  Seek out your wildest fantasies :-)
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

> am :: AudSF (Double,Double,Double,Double) Double
> am = proc (c,m,a,offset) -> do
>              cosM <- oscil f1 0.25 -< m -- cos x = sin (2*pi*x-pi/2)
>              cosC <- oscil f1 0.25 -< c -- cos x = sin (2*pi*x-pi/2)
>              returnA -< cosC * (offset + a * cosM)

=================
Test/Use Examples
=================

Again, I'm creating a useful operator for sending 4 signals into "am" at once:

> (>>>>>) :: Clock p => (Signal p a b, Signal p a c, Signal p a d, Signal p a e) -> Signal p (b,c,d,e) f -> Signal p a f
> (sf1, sf2, sf3,sf4) >>>>> sf5 = 
>     proc a -> do
>         b <- sf1 -< a
>         c <- sf2 -< a
>         d <- sf3 -< a
>         e <- sf4 -< a
>         f <- sf5 -< (b,c,d,e)
>         returnA  -< f

> test_am :: IO ()
> test_am2 :: IO ()
> test_am   = outFile "am.wav" 9.0 (((constA 440, (constA 440 >>> line 0 1000 9), constA 1, constA 1) >>>>> am) >>> (arr (/4)))
> test_am2  = outFile "am2.wav" 9.0 (((constA 440, (constA 440 >>> line 0 20  9), constA 1, constA 1) >>>>> am) >>> (arr (/4)))
> test_am3  = outFile "am3.wav" 9.0 (((constA 440, constA 0, constA 1, constA 0) >>>>> am) >>> (arr (/4)))


------------------------------------------------------------------------------------------------
End of Assignment 3
------------------------------------------------------------------------------------------------