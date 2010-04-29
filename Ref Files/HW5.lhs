------------------------------------------------------------------------------------------------
Nathan Griffith
Homework 5:	Subtractive Synthesis
CS 432
Sunday November 8
------------------------------------------------------------------------------------------------

> {-# LANGUAGE Arrows #-}
> import Euterpea hiding (line,delay)
> import Euterpea.Audio.Basics
> import Euterpea.Audio.CSoundGenerators
> import Euterpea.Audio.Types
> import Euterpea.Audio.IO
> import Euterpea.Audio.Render
> import Prelude hiding (init)
> import Control.Arrow hiding (returnA)
> import FurElise


------------------------------------------------------------------------------------------------
Declare some useful types and functions:
------------------------------------------------------------------------------------------------

> type AudSF a b = Signal AudRate a b

> f1 = gen10 8192 [1]	
> f2 = gen10 4096 [1, 0.5, 0.333, 0.25, 0.2, 0.166, 0.142, 0.125, 
>                  0.111, 0.1, 0.09, 0.083, 0.076, 0.071, 0.066, 0.062]

> frereJacques =
>   let part1 = (c 5 (1/4)) :+: (d 5 (1/4)) :+: (e 5 (1/4)) :+: (c 5 (1/4))
>       part2 = (e 5 (1/4)) :+: (f 5 (1/4)) :+: (g 5 (1/2))
>       part3 = (g 5 (1/8)) :+: (a 5 (1/8)) :+: (g 5 (1/8)) :+: (f 5 (1/8)) :+: (e 5 (1/4)) :+: (c 5 (1/4))
>       part4 = (c 5 (1/4)) :+: (g 4 (1/4)) :+: (c 5 (1/2)) 
>       dub a = a :+: a
>   in tempo 2 (dub part1 :+: dub part2 :+: dub part3 :+: dub part4)

------------------------------------------------------------------------------------------------
Start of problems:
------------------------------------------------------------------------------------------------

**************************************************************************************
1. Define a function:
    sweep :: Double -> AudSF () Double
such that sweep bw yields a signal function that:
	-feeds white noise into a bandpass filter realized using the reson opcode,
	-sweeps the center frequency using an exponential curve from 100 Hz to 10 kHz, and
	-uses bw as the filter bandwidth.
Test your function to see the effect of varying the bandwidth of the filter.
**************************************************************************************

> sweep :: Double -> AudSF () Double
> sweep bw = 
>      let pitch = 2
>      in proc _ -> do
>        noise <- rand 2 -< 1
>        center <- expon 100 3 10000 -< 1
>        out <- reson 1 -< (noise,center,bw)
>        returnA -< out

> sweepOut10 :: IO ()
> sweepOut10 = outFile "sweep10.wav" 3.0 (sweep 10)
> sweepOut100 :: IO ()
> sweepOut100 = outFile "sweep100.wav" 3.0 (sweep 100)
> sweepOut1000 :: IO ()
> sweepOut1000 = outFile "sweep1000.wav" 3.0 (sweep 1000)

The smaller the bandwidth, the less noisy and more intelligible the pitch is.

**************************************************************************************
2. As a variation of sweep, define a function:
    wind :: AudSF () Double
that gives your best rendition of the wind howling on a Halloween night.
**************************************************************************************

> helper :: Double -> Double -> Double -> Double -> AudSF () Double
> helper bw dur start stop = 
>      let pitch = 2
>      in proc _ -> do
>        noise <- rand 2 -< 1
>        center <- expon start dur stop -< 1
>        out <- reson 1 -< (noise,center,bw)
>        returnA -< out

> wind :: AudSF () Double
> wind =
>      proc _ -> do
>        sweep1 <- helper 10 10 10 900 -< ()
>        sweep2 <- helper 100 10 10000 5000 -< ()
>        sweep3 <- helper 1000 10 10 20000 -< ()
>        sweep4 <- helper 100 10 10000 100 -< ()
>        sweep5 <- helper 1000 10 8000 15000 -< ()
>        sweep6 <- helper 100 10 10000 900 -< ()
>        returnA -< (sweep1 + sweep2 + sweep3 + sweep4 + sweep5 + sweep6)/6

> windOut :: IO ()
> windOut = outFile "wind.wav" 10.0 wind

**************************************************************************************
3. Let's use a combination of FM, enveloping, and subtractive synthesis to build a 
guitar-like instrument.  Do this in four steps, as follows:
	-First create a pitched signal with a rich source of harmonics by using the buzz 
    op-code, where the number of harmonics is set to one-half the sampling rate 
    divided by the fundamental frequency (to avoid aliasing).  
    Note:  Although we know that the audio sampling rate is 44,100 Hz, you can make 
    your code more abstract by writing the following to determine the over-loaded 
    sampling rate:
      let sr = rate (undefined :: AudRate)
	-Add a little more richness to the signal by adding a small amount of vibrato.
	-Now create an amplitude envelope that suitably creates a "plucking" effect.
	-Finally, run the signal through a low-pass filter using the tone or butterlp 
    op-codes whose cut-off frequency is initially very high, and then drops to a 
    lower frequency that is a function of the pitch (so that low notes and high 
    notes have similar timbre).  This reflects the fact that the higher partials 
    die off more quickly than the lower ones.
**************************************************************************************

> guitarLike :: Instr (AudSF () Double)
> guitarLike dur pch vol _ =
>     let sr = rate (undefined :: AudRate)
>         freq = apToHz pch
>         d = fromRational dur
>         vibFreq = 5
>         vibDepth = 3
>     in proc _ -> do
>          mod <- oscil f1 0 -< vibFreq
>          sig <- buzz f1 0 -< (freq + vibDepth * mod, floor ((sr/2)/freq))
>          env <- linseg [0,1,0.9,0] [d*0.01,d*0.01,d*0.98] -< ()
>          cutoff <- expon 15000 d (freq/2.0) -< 1
>          filtered <- butterlp -< (sig * env,cutoff)
>          returnA -< filtered



**************************************************************************************
4. Test your guitar-like sound by playing the first eight bars of Beethoven's Fur 
Elise (see below) using your instrument.  You are welcome to use p-fields to 
control your sound, but remember that the first three arguments in the Instr 
interface are Dur, AbsPitch, and Volume, which should be all that you need.
**************************************************************************************

> guitarLikeInstr :: InstrumentName
> guitarLikeInstr = Custom "A guitar-like instrument"

> myInstrMap :: InstrMap (AudSF () Double)
> myInstrMap = [(guitarLikeInstr, guitarLike)]

> recordGuitar = uncurry (outFile "guitar.wav") (renderSF (instrument guitarLikeInstr (tempo (1/2) (c 5 (4/4)))) myInstrMap)


> guitarFurElise = uncurry (outFile "guitar.wav") (renderSF (instrument guitarLikeInstr furElise) myInstrMap)
