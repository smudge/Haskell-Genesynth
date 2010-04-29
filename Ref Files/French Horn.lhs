------------------------------------------------------------------------------------------------
Nathan Griffith
Final Project: French Horn Synth
CS 432
Sunday November 15
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
> import Control.CCA.Types

------------------------------------------------------------------------------------------------
Declare some useful types and functions:
------------------------------------------------------------------------------------------------

> type AudSF a b = Signal AudRate a b

> flute :: Instr (AudSF () Double)
> flute durR pch vol (lfofreq:_) = 
>     let press = 0.93
>         breath = 0.02
>         freq   = apToHz pch
>         dur    = fromRational durR
>         amp    = fromIntegral vol / 127 * 0.35
>         env1    = linseg [0, 1.1*press, press, press*0.9, 0] 
>                          [0.06, 0.2, dur-0.31, 0.05] 
>         env2    = linseg [0, 1, 1, 0] [0.01, dur-0.02, 0.01]
>         envibr  = linseg [0, 0, 1, 1] [0.5, 0.5, dur-1]
>     in proc _ -> do
>          amp1 <- env1 -< ()
>          amp2 <- env2 -< ()
>          ampv <- envibr -< ()
>          flow <- rand 1 -< amp1
>          vibr <- oscils lfofreq -< 0.1 * ampv
>          rec   
>            let feedbk = body * 0.4
>            body <- delay (1/freq)   -< out
>            x    <- delay (1/freq/2) -< breath*flow +amp1+vibr+feedbk
>            out  <- tone             -< (x - x*x*x + feedbk, 2000)
>          returnA -< out * amp * amp2

> theFlute :: InstrumentName
> theFlute = Custom "A Flute"
> fluteInstrMap :: InstrMap (AudSF () Double)
> fluteInstrMap = [(theFlute, flute)]

> frereJacques =
>   let part1 = (c 5 (1/8)) :+: (d 5 (1/8)) :+: (e 5 (1/8)) :+: (c 5 (1/8))
>       part2 = (e 5 (1/8)) :+: (f 5 (1/8)) :+: (g 5 (1/4))
>       part3 = (g 5 (1/16)) :+: (a 5 (1/16)) :+: (g 5 (1/16)) :+: (f 5 (1/16)) :+: (e 5 (1/8)) :+: (c 5 (1/8))
>       part4 = (c 5 (1/8)) :+: (g 4 (1/8)) :+: (c 5 (1/4)) 
>       dub a = a :+: a
>   in (dub part1 :+: dub part2 :+: dub part3 :+: dub part4)

> mult a 1 = a
> mult a n = a :+: mult a (n-1)

> addPFields :: [Double] -> Music Pitch -> Music (Pitch, [NoteAttribute])
> addPFields pf = mMap (\p -> (p, [PFields pf]))

> genapf :: AudSF Double Double -> (Double, Double) -> AudSF Double Double
> genapf (sf) (g,d) =
>   proc ain -> do
>     rec a1 <- delay (d/1000) -< ain + g * aout
>         a2 <- sf -< a1
>         let aout = a2 - g * ain
>     returnA -< aout

> apf :: (Double, Double) -> AudSF Double Double
> apf = genapf (arr id)

> snapf :: (Double, Double) -> (Double, Double) -> AudSF Double Double
> snapf = genapf . apf

> dnapf :: (Double, Double) -> (Double, Double) -> (Double, Double) -> AudSF Double Double
> dnapf (g3, d3) (g2, d2) (g1, d1) = genapf (apf (g2, d2) >>> apf (g3, d3)) (g1, d1)


> mediumReverb :: AudSF Double Double
> mediumReverb =
>     proc ain -> do
>        rec   
>          ainlow <- init 0 <<< butterlp -< (ain,6000)
>          a1 <- dnapf (0.25, 35) (0.35, 8.3) (0.25, 35) -< ainlow + fbk*0.5
>          a2 <- delay 0.067 <<< apf (0.45, 30) <<< delay 0.005 -< a1
>          a2delayed <- init 0 <<< delay 0.015 -< a2
>          a3 <- snapf (0.25, 39) (0.35, 9.8) <<< delay 0.015 -< a2delayed * 0.4 + ainlow
>          fbk <- delay 0.108 <<< init 0 <<< butterbp -< (a3 * 0.4, 500, 500)
>          let aout = a1 * 0.5 + a2 * 0.5 + a3 * 0.5
>        returnA -< aout

> smallRmRvb g (g5,t5) (g4,t4) (g3,t3) (g2,t2) (g1,t1) =
>   proc ain -> do
>   rec a1  <- butterlp                      -< (ain,6000)
>       mid <- dnapf (g3,t3) (g2,t2) (g1,t1) -< a1 + g*fbk
>       fbk <- init 0 <<< butterbp           -< (end*0.5,500,500)
>       end <- snapf (g5,t5) (g4,t4)         -< mid
>   returnA -< mid*0.5 + end*0.5

> smallReverb = smallRmRvb 0.4 (0.3, 30) (0.08,66) (0.3, 8.3) (0.25,22) (0.15,35)


> constA :: Clock p => a -> Signal p () a
> constA = arr.const

------------------------------------------------------------------------------------------------
Start of French Horn Synth!:
------------------------------------------------------------------------------------------------

********************************
FREQUENCY DATA (and other useful table data):
********************************


> func 1 =  gen09' 4097 [(1, 1.0, 0)]
> func 10 = gen09' 4    [(1, 0.0, 0)]
> func 11 = gen09' 4097 [(2, 6.236, 0), (3, 12.827, 0)]
> func 12 = gen09' 4097 [(4, 21.591, 0), (5, 11.401, 0), (6, 3.570, 0), (7, 2.833, 0)]
> func 13 = gen09' 4097 [(8, 3.070, 0), (9, 1.053, 0), (10, 0.773, 0), (11, 1.349, 0), (12, 0.819, 0), (13, 0.369, 0), (14, 0.362, 0), (15, 0.165, 0), (16, 0.124, 0), (18, 0.026, 0), (19, 0.042, 0)]
> func 14 = gen09' 4097 [(2, 3.236, 0), (3, 6.827, 0)]
> func 15 = gen09' 4097 [(4, 5.591, 0), (5, 2.401, 0), (6, 1.870, 0), (7, 0.733, 0)]
> func 16 = gen09' 4097 [(8, 0.970, 0), (9, 0.553, 0), (10, 0.373, 0), (11, 0.549, 0), (12, 0.319, 0), (13, 0.119, 0), (14, 0.092, 0), (15, 0.045, 0), (16, 0.034, 0)]
> func 17 = gen09' 4097 [(2, 5.019, 0), (3, 4.281, 0)]
> func 18 = gen09' 4097 [(4, 2.091, 0), (5, 1.001, 0), (6, 0.670, 0), (7, 0.233, 0)]
> func 19 = gen09' 4097 [(8, 0.200, 0), (9, 0.103, 0), (10, 0.073, 0), (11, 0.089, 0), (12, 0.059, 0), (13, 0.029, 0)]
> func 20 = gen09' 4097 [(2, 4.712, 0), (3, 1.847, 0)]
> func 21 = gen09' 4097 [(4, 0.591, 0), (5, 0.401, 0), (6, 0.270, 0), (7, 0.113, 0)]
> func 22 = gen09' 4097 [(8, 0.060, 0), (9, 0.053, 0), (10, 0.023, 0)]
> func 23 = gen09' 4097 [(2, 1.512, 0), (3, 0.247, 0)]
> func 24 = gen09' 4097 [(4, 0.121, 0), (5, 0.101, 0), (6, 0.030, 0), (7, 0.053, 0)]
> func 25 = gen09' 4097 [(8, 0.030, 0)]
> func 26 = gen09' 4097 [(2, 0.412, 0), (3, 0.087, 0)]
> func 27 = gen09' 4097 [(4, 0.071, 0), (5, 0.021, 0)]
> func 28 = gen09' 4097 [(2, 0.309, 0), (3, 0.067, 0)]
> func 29 = gen09' 4097 [(4, 0.031, 0)]
> func 30 = gen09' 4097 [(2, 0.161, 0), (3, 0.047, 0)]

> func2 2 = [0, 40, 80, 160, 320, 640, 1280, 2560, 5120, 10240]
> func2 3  = [11, 12, 13, 52.476, 14, 15, 16, 18.006, 17, 18, 19, 11.274, 20, 21, 22, 6.955, 23, 24, 25, 2.260, 26, 27, 10, 1.171, 28, 29, 10, 1.106, 30, 10, 10, 1.019]

> doStepwiseFunction ifreq -- formerly func 4
>    | ifreq >= 0   && ifreq < 85 = 0
>    | ifreq >= 85  && ifreq < 114 = 1
>    | ifreq >= 114 && ifreq < 153 = 2
>    | ifreq >= 153 && ifreq < 204 = 3
>    | ifreq >= 204 && ifreq < 272 = 4
>    | ifreq >= 272 && ifreq < 364 = 5
>    | ifreq >= 364 && ifreq < 486 = 6
>    | ifreq >= 486 = 7
>    | otherwise = 0

f4 =  gen17' 2048 [0, 0, 85, 1, 114, 2, 153, 3, 204, 4, 272, 5, 364, 6, 486, 7]

*************
THE HORN
*************

> horn :: Instr (AudSF () Double)
> horn durR pch vol (p1:p2:p3:p4:p5:_) =
>     let ifreq = apToHz pch
>         dur = (fromRational durR) * p5
>         iamp = fromIntegral vol / 127 * 0.35 * 2
>         ivibd = p1*ifreq/100.0 -- vibrato depth
>         iatt = p2
>         idec = p3
>         isus = dur-iatt-idec-0.005
>         ifcut = (func2 2) !! (round p4) -- once handled f2, now does a table lookup
>         irange = doStepwiseFunction ifreq
>         iwt2 = (func2 3) !! (round (irange*4)) -- wavetable numbers
>         iwt3 = (func2 3) !! (round ((irange*4)+1)) 
>         iwt4 = (func2 3) !! (round ((irange*4)+2))
>         inorm = (func2 3) !! (round ((irange*4)+3)) -- this normalizes after adding it all together
>         ivibr1 = 2.5
>         ivibr2 = 4.5 
>     in proc _ -> do
>       rec 
>         amp1 <- linseg [0, 0, 0.5, 0.9, 1, 0.9,  0, 0] [0.001, 0.5*iatt, 0.5*iatt, 0.5*isus, 0.5*isus, 0.5*idec, 0.3, 0.5*idec,1] -< ()
>         let amp2 = amp1*amp1 -- the envelopes are raised to varying degrees, depending on the group number
>             amp3 = amp2*amp1 
>             amp4 = amp3*amp1 
>         kvidb <- linseg [0.1,1,0.7] [0.8*dur,0.2*dur] -< () -- VIBRATO!
>         kvrate <- linseg [ivibr1, ivibr2] [dur] -< () -- vibrato rate..
>         kvib <- oscil (func 1) 0 -< kvrate
>         let vibrato = kvib*kvidb*ivibd
>         awt1 <- oscil (func 1) 0 -< ifreq+vibrato -- do the wavetable lookups and start to generate SOUND!
>         awt2 <- oscil (func iwt2) 0 -< ifreq+vibrato
>         awt3 <- oscil (func iwt3) 0 -< ifreq+vibrato
>         awt4 <- oscil (func iwt4) 0 -< ifreq+vibrato
>         let asig = (awt1*amp1+awt2*amp2+awt3*amp3+awt4*amp4)*iamp/inorm
>         afilt	<- tone	-< (asig, ifcut)			-- low pass filter to controll brightness
>         asig2	<- balance 1 -< (afilt, asig)			-- balance the new signal with regards to the old one (possibly not needed due to OutFileNorm)
>       returnA -< asig2

> myHorn :: InstrumentName
> myHorn = Custom "A Waveguide French Horn"
> myInstrMap :: InstrMap (AudSF () Double)
> myInstrMap = [(myHorn, horn)]

> pFieldz = [0.5, 0.04, 0.04, 9, 1]
> pFieldz2 = [1.5, 0.04, 0.04, 1, 1]


> beat = (c 4 (1/4)):+:(g 4 (1/2)):+:(rest (1/4)) --not used, was once useful for quick renderings

> recordNote = uncurry (outFileNorm "horn.wav") (renderSF (instrument myHorn (addPFields pFieldz2 (c 4 (1/4)))) myInstrMap)


********************
FRERE JACQUES (BASIC SOUND TEST)
********************


> recordHorn = uncurry (outFileNorm "horn.wav") (renderSF (instrument myHorn (addPFields pFieldz2 frereJacques)) myInstrMap)
> recordHornVerb = uncurry (outFileNorm "hornVerb.wav") (head (map (\(x,y) -> (x+2,mediumReverb <<< y)) [(renderSF (instrument myHorn (addPFields pFieldz2 frereJacques)) myInstrMap)]))
> recordHornVerbSmall = uncurry (outFileNorm "hornVerbSmall.wav") (head (map (\(x,y) -> (x+2,smallReverb <<< y)) [(renderSF (instrument myHorn (addPFields pFieldz2 frereJacques)) myInstrMap)]))

********************
RangeTest
********************

> pRange = [0.5, 0.04, 0.04, 5, 1]

> rangeTest = 
>   let rangeTestUp 72 = Prim (Note (1/8) (pitch 72))
>       rangeTestUp n = Prim (Note (1/8) (pitch n)) :+: rangeTestUp (n+1)
>       rangeTestDn 38 = Prim (Note (1/8) (pitch 38))
>       rangeTestDn n = Prim (Note (1/8) (pitch n)) :+: rangeTestDn (n-1)
>   in (rangeTestUp 38) :+: (rangeTestDn 72)

> recordRange = uncurry (outFileNorm "range.wav") (renderSF (instrument myHorn (addPFields pRange rangeTest)) myInstrMap)

********************
MOZART: Horn Concerto No. 4, Rondo
********************

> pMozart = [0.5, 0.02, 0.02, 9, 0.9]
> pMozartDark = [0.5, 0.02, 0.02, 1, 0.9]
> pMozartVerb = [0.5, 0.04, 0.04, 7, 0.7]


> mozart =
>   let mm1 = (bf 4 (1/8))
>       mm2 = mult (ef 5 (1/8)) 6
>       mm3 = (d 5 (1/8)):+:(f 5 (1/8)):+:(bf 5 (1/8)):+:(bf 5 (1/8)):+:(af 5 (1/8)):+:(f 5 (1/8))
>       mm4 = (mult (ef 5 (1/8)) 4) :+: (d 5 (1/8)):+:(ef 5 (1/8))
>       mm5 = (f 5 (1/8)) :+: (mult (bf 4 (1/8)) 2) :+: (bf 4 (1/4))
>       mm6n7 = (g 5 (1/8)) :+: (bf 5 (1/8)) :+: (ef 5 (3/32)) :+: (g 5 (1/32)) :+: (f 5 (1/8)) :+: (ef 5 (1/8)) :+: (f 5 (1/8)) :+: (ef 5 (1/8)) :+: (mult (ef 4 (1/8)) 2) :+: (ef 4 (3/8))
>   in (rest (5/8)) :+: mm1 :+: mm2 :+: mm3 :+: mm4 :+: mm5 :+: mm1 :+: mm2 :+: mm3 :+: mm6n7

> recordMozart = uncurry (outFileNorm "mozart.wav") (renderSF (instrument myHorn (addPFields pMozart (tempo 1.5 mozart))) myInstrMap)
> recordMozartDark = uncurry (outFileNorm "mozartDark.wav") (renderSF (instrument myHorn (addPFields pMozartDark (tempo 1.5 mozart))) myInstrMap)
> recordMozartVerb = uncurry (outFileNorm "mozartVerb.wav") (head (map (\(x,y) -> (x+2,mediumReverb <<< y)) [(renderSF (instrument myHorn (addPFields pMozartVerb (tempo (1.5) mozart))) myInstrMap)]))


********************
Strauss: Horn Concerto 1
********************

> pStrauss = [1, 0.04, 0.02, 1, 1]

> strauss =
>   let mm1 = (rest (3/4)):+:(bf 4 (1/4))
>       mm2 = (bf 5 (3/4)):+:(g 5 (1/4))
>       mm3 = (ef 5 (3/4)):+:(ef 5 (1/4))
>       mm4 = (af  5 (1/4)):+:(g  5 (1/4)):+:(f 5 (1/4)):+:(ef 5 (7/32)):+:(f 5 (1/32))
>       mm5 = (ef 5 (1/4)):+:(d  5 (15/32)):+:(rest (1/32)):+:(bf 4 (1/4))
>       mm6 = (ef 5 (2/4)):+:(f 5 (7/32)):+:(rest (1/32)):+:(bf 4 (1/4))
>       mm7 = (g 5 (2/4)):+:(af 5 (1/4)):+:(a 5 (1/4))
>       mm8 = (bf 5 (3/8)):+:(rest (1/8)):+:(c 6 (1/2))
>       mm9 = (f 5 (11/16)):+:(rest (1/16)):+:(bf 4 (1/4))
>       mma = (ef 5 (1/4)):+:(d  5 (15/32)):+:(rest (1/32)):+:(f 5 (1/4))
>       mmb = (bf 5 (1/2)):+:(af 5 (1/2))
>       mmc = (g  5 (1/4)):+:(bf  5 (1/4)):+:(ef 6 (3/4)):+:(bf 5 (1/2)):+:(bf 5 (1/4))
>       mmd = (g  5 (1/4)):+: (g  5 (1/8)):+:(f  5 (1/8)):+: (ef  5 (3/4)):+: (d  5 (1/4))
>       mme = (ef 5 (3/4)):+:(rest (1/4))
>   in mm1:+:mm2:+:mm3:+:mm4:+:mm5:+:mm6:+:mm7:+:mm8:+:mm9:+:mm2:+:mm3:+:mm4:+:mma:+:mmb:+:mmc:+:mmd:+:mme

> recordStrauss = uncurry (outFileNorm "strauss.wav") (renderSF (instrument myHorn (addPFields pStrauss strauss)) myInstrMap)
> recordStraussVerb = uncurry (outFileNorm "straussVerb.wav") (head (map (\(x,y) -> (x+2,mediumReverb <<< y)) [(renderSF (instrument myHorn (addPFields pStrauss strauss)) myInstrMap)]))
