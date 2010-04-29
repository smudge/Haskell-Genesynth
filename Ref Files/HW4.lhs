------------------------------------------------------------------------------------------------
Nathan Griffith
Homework 4
CS 432
October 28, 2009
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

------------------------------------------------------------------------------------------------
Declare some useful types and functions:
------------------------------------------------------------------------------------------------

> type AudSF a b = Signal AudRate a b

> f1 = gen10 8192 [1]	
> f2 = gen10 4096 [1, 0.5, 0.333, 0.25, 0.2, 0.166, 0.142, 0.125, 
>                  0.111, 0.1, 0.09, 0.083, 0.076, 0.071, 0.066, 0.062]

> wave0 :: Table
> wave0 = compSine1 1024 [0.158, 0.316, 1.0, 1.0, 0.282, 0.112, 
>                         0.063, 0.079, 0.126, 0.071]

> manySines = compSine2 8192 [(0.5, 0.9, 0.0),
>                     (1.0, 1.0, 0.0), (1.1, 0.7, 0.0), (2.0, 0.6, 0.0),
>                     (2.5, 0.3, 0.0), (3.0, 0.33, 0.0), (5.0, 0.2, 0.0)]

> vibrato :: Double -> Double -> AudSF Double Double
> vibrato f d = 
>     proc freq -> do
>       mod <- oscil f1 0 -< f
>       oscil f1 0 -< (freq + d * mod) --shouldn't need normalization

> frereJacques =
>   let part1 = (c 5 (1/4)) :+: (d 5 (1/4)) :+: (e 5 (1/4)) :+: (c 5 (1/4))
>       part2 = (e 5 (1/4)) :+: (f 5 (1/4)) :+: (g 5 (1/2))
>       part3 = (g 5 (1/8)) :+: (a 5 (1/8)) :+: (g 5 (1/8)) :+: (f 5 (1/8)) :+: (e 5 (1/4)) :+: (c 5 (1/4))
>       part4 = (c 5 (1/4)) :+: (g 4 (1/4)) :+: (c 5 (1/2)) 
>       dub a = a :+: a
>   in tempo 2 (dub part1 :+: dub part2 :+: dub part3 :+: dub part4)

> oscil1 :: Clock p => 
>           Table                   
>        -> Double                  
>        -- ^ delay in seconds before 'oscil1' incremental sampling begins
>        -> Double                  
>        -- ^ duration in seconds to sample through the table just once.
>        -> Signal p Double Double
> oscil1 table del dur =
>     proc amp -> do
>       a <-oscil table 0 -< (1.0/dur)
>       returnA -< a * amp


------------------------------------------------------------------------------------------------
Start of problems:
------------------------------------------------------------------------------------------------

+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
1. Define at least two instruments on your own that create sounds that you like.  
Each instrument should take as arguments a pitch and duration, plus at least 
two p-fields that control some parameters of the sound that you are interested 
in.  You may include more p-fields of course.
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

> spookyFifths :: Instr (AudSF () Double)
> spookyFifths dur pch vol (freq1:freq2:depth1:depth2:_) =
>     let d = fromRational dur
>         env = linseg [0, 1, 1, 0] [d/3.0, d/3.0, d/3.0] 
>     in proc _ -> do
>       amp <- env -< ()
>       b <- vibrato freq1 depth1 -< apToHz pch
>       c <- vibrato freq1 depth2 -< apToHz (pch + 7)
>       returnA -< (b * amp + c * amp) / 2
>
> anotherInstrument :: Instr (AudSF () (Double,Double))
> anotherInstrument dur pch vol (delay1:delay2:_) = 
>      let pitch = apToHz pch
>          d = fromRational dur
>          env = linseg [0, 1, 0, 1, 0.5, 1, 0, 1, 0] [d/8.0, d/8.0, d/8.0, d/8.0, d/8.0, d/8.0, d/8.0, d/8.0] 
>      in proc _ -> do
>        amp <- env -< ()
>        s1 <- oscil f1 0 -< pitch
>        s2 <- oscil f2 0 -< pitch
>        d1 <- delay delay1 -< s1* amp
>        d2 <- delay delay2 -< s2 * amp
>        returnA -< ((s2 + d1)/2,(s1 + d2)/2)

> mySpookyFifths :: InstrumentName
> mySpookyFifths = Custom "some spooky fifths!"
> mySecondInstrument :: InstrumentName
> mySecondInstrument = Custom "another one!"



+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Using the instruments defined above, show off their beautiful sound by playing 
them to a tune of your choice, such as Freres Jacques from an earlier assignment.  
You will have to create an "InstrMap" to make this work.
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


> myInstrMap :: InstrMap (AudSF () Double)
> myInstrMap = [(mySpookyFifths, spookyFifths)]

> myInstrMap2 :: InstrMap (AudSF () (Double, Double))
> myInstrMap2 = [(mySecondInstrument, anotherInstrument)]

> addPFields :: [Double] -> Music Pitch -> Music (Pitch, [NoteAttribute])
> addPFields pf = mMap (\p -> (p, [PFields pf]))

> recordSpooky = uncurry (outFile "spooky.wav") (renderSF (instrument mySpookyFifths (addPFields [6.0,1.0,5.0,10.0] frereJacques)) myInstrMap)

> recordSecondInstr = uncurry (outFile "second.wav") (renderSF (instrument mySecondInstrument (addPFields [0.1,0.05] frereJacques)) myInstrMap2)


+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Implement in Euterpea the essence of the dynamic spectrum instrument 1104 shown 
in Figures 11.9 and 11.10 in Chapter 11 of TcB (and see below).  This is a largish 
and somewhat complex instrument -- I suggest implementing small sections at a time 
(as different signal functions), and then "wiring them together" to get the final result.
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

> instr1104 :: Instr (AudSF () (Double, Double))
> instr1104 dur pch vol (p1:p2:p3:p4:p5:p6:p7:p8:p9:p10:p11:p12:p13:p14:p15:p16:p17:p18:p19:p20:p21:p22:p23:p24:p25:p26:p27:p28:p29:p30:p31:p32:p33:p34:_) =
>     let idur = p3
>         iamp = p4
>         ifreq = p5
>         iatt = p6
>         idec = p7
>         imaxaf = p8/100.00
>         imaxff = p9/100.00
>         isr2 = sqrt 2.0
>         isr2b2 = isr2/2.0
>         imaxpan = 2
>         ipanfunc = p34
>         component1 = component p10 imaxaf p12 (ifreq*p11) imaxff p13 idur
>         component2 = component p14 imaxaf p16 (ifreq*p15) imaxff p13 idur
>         component3 = component p18 imaxaf p20 (ifreq*p19) imaxff p13 idur
>         component4 = component p22 imaxaf p24 (ifreq*p23) imaxff p13 idur
>         component5 = component p26 imaxaf p28 (ifreq*p27) imaxff p13 idur
>         component6 = component p30 imaxaf p32 (ifreq*p31) imaxff p13 idur
>         iampsum   =  p10+p14+p18+p22+p26+p30
>     in proc _ -> do
>         a1 <- component1 -< ()
>         a2 <- component2 -< ()
>         a3 <- component3 -< ()
>         a4 <- component4 -< ()
>         a5 <- component5 -< ()
>         a6 <- component6 -< ()
>         kenv <- linen iatt idur idec -< iamp
>         kpan <- oscil1 (func ipanfunc) 0 idur -< imaxpan           -- PANNING TRAJECTORY
>         let ktemp = sqrt(1+kpan*kpan)
>             kpleft  | (kpan >= -1) && (kpan <= 1) = isr2b2*(1-kpan)/ktemp
>                     |  kpan < -1 = 2.0/(1+kpan*kpan)
>                     |  kpan >  1 = 0
>             kpright | (kpan >= -1) && (kpan <= 1) = isr2b2*(1+kpan)/ktemp
>                     |  kpan >  1 = 2.0/(1+kpan*kpan)
>                     |  kpan < -1 = 0
>             asig = (kenv*(a1+a2+a3+a4+a5+a6)/(iampsum))/32767
>         returnA -< ((asig*kpleft),(asig*kpright))

> myInstr1104 :: InstrumentName
> myInstr1104 = Custom "Instrument 1104"
> myInstrMap1104 :: InstrMap (AudSF () (Double, Double))
> myInstrMap1104 = [(myInstr1104, instr1104)]

> component :: Double -> Double -> Double -> Double -> Double -> Double -> Double -> AudSF () Double
> component iramp1 imaxaf iafunc1 ifreq1 imaxff iffunc1 idur = 
>     let imaxaf1 = iramp1 * imaxaf
>         imaxff1 = ifreq1 * imaxff
>     in proc _ -> do
>          kampf1  <- oscil1 (func iafunc1) 0 idur -< imaxaf1
>          kfreqf1 <- oscil1 (func iffunc1) 0 idur -< imaxff1
>          a1 <- oscili (func 1) 0 -< (ifreq1+kfreqf1)
>          returnA -< a1 * (iramp1+kampf1)


+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Demonstrate your success in (3) above by implementing as a Music value the essence 
of 1104.sco (see below).
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

> --SINEWAVE
> func 1 = gen10 8192 [1]	
> --FIRST COMPONENT
> func 11 = gen09 1024 [(0.5, 1, 55),	(1.3, 2.3, 40), (2.6, 1.32, 35), (3.45, 3, 20)]				   --AMPLITUDE
> func 12 = gen09 1024 [(1, 1, 90),	(1.3, 2.3, 84),  (1.6, 1.32, 75),	(2.45, 3, 60)]				   --FREQUENCY
> --SECOND COMPONENT
> func 21 = gen09 1024	[(0.7, 3, 66), (1.63, 2.2, 37), (3.36, 4.32, 16), (1.45, 3, 12)]			   --AMPLITUDE
> func 22 = gen09 1024	[(1, 1, 10), (2.6, 1.13, 84), (0.8, 2.46, 75), (4.9, 1.5, 60)]			   --FREQUENCY
> --THIRD COMPONENT
> func 31 = gen09 1024	[(2.1, 1, 22), (4.18, 6.6, 111), (1.12, 1.1, 5), (5.15, 9, 36)]			   --AMPLITUDE
> func 32 = gen09 1024	[(1, 1, 79), (2.6, 1.13, 84),	(0.8, 2.46, 75), (4.9, 1.5, 60)]			   --FREQUENCY
> --FOURTH COMPONENT
> func 41 = gen09 1024	[(6.62, 2.5, 44), (7.1, 2.2, 48),	(2.89, 3.5, 1)]					   --AMPLITUDE
> func 42 = gen09 1024	[(0.2, 1.6, 179), (2.55, 1.2, 4),	(0.16, 4.12, 123)]					   --FREQUENCY
> --FIFTH COMPONENT
> func 51 = gen09 1024	[(8.4, 0.25, 188),  (1.02, 9.9, 8), (4.48, 4.4, 100), (1.37, 2.25, 90)]		   --AMPLITUDE
> func 52 = gen09 1024	[(0.25, 4, 79), (5.4, 0.42, 184), (0.8, 9.12, 57), (1.24, 6, 6)]			   --FREQUENCY
> --SIXTH COMPONENT
> func 61 = gen09 1024	[(1.62, 0.25, 8),  (5.1, 9.9, 180),	(0.89, 4.4, 1), (3, 2.4, 30),  (6.85, 2.25, 270)]  --AMPLITUDE
> func 62 = gen09 1024	[(1.25, 4, 79),	(2.55, 2.1, 48), (0.16, 9.12, 37), (2.4, 3, 7),  (5.96, 6, 160)]	   --FREQUENCY
> --PANNING FUNCTIONS
> func 71 = gen10 1024 [1, 1, 1, 1, 1, 1, 1, 1, 1, 1]
> func 72 = gen10 1024 [0, 0, 0, 1, 0, 1, 0, 1, 0, 1]
> func 73 = gen09 1024 [(1.5, 0.8, 180), (2.4, 0.78, 170), (3.5, 0.8, 160), (4.6, 0.65, 140)]
> func 74 = gen09 1024 [(1.3, 0.5, 170), (3.34, 0.6, 190), (4.7, 0.7, 140), (5.33, 0.35, 200)]


> sco 1  = [0, 0, 5, 7000, 100, 1, 1.5, 20, 5, 1, 1, 11, 12, 0.86, 2.01, 21, 22, 0.77, 3.02, 31, 32, 0.68, 4.03, 41, 42, 0.59, 5.04, 51, 52, 0.5, 6.05, 61, 62, 11] 
> sco 2  = [0, 2, 7, 11000, 107, 2, 1.5, 20, 10, 0.05, 1, 12, 11, 0.68, 1.35, 22, 21, 0.79, 1.78, 32, 31, 0.67, 2.13, 42, 41, 0.59, 2.55, 52, 51, 0.82, 3.23, 62, 61, 74]
> sco 3  = [0, 5, 7, 11000, 222, 2, 1.5, 20, 10, 0.7, 1, 12, 11, 0.68, 1.35, 22, 21, 0.79, 1.78, 32, 31, 0.67, 2.13, 42, 41, 0.59, 2.55, 52, 51, 0.82, 3.23, 62, 61, 22]
> sco 4  = [0, 5.59, 4.35, 4000, 4316, 1.5, 2, 30, 8, 0.8, 1, 12, 11, 0.7, 1.35, 61, 62, 0.6, 1.78, 52, 52, 0.5, 1.13, 22, 22, 0.55, 1.55, 41, 42, 0.76, 1.23, 32, 31, 73]
> sco 5  = [0, 5.6, 10.3, 11000, 62, 3.5, 3, 20, 10, 0.97, 1, 12, 11, 0.79, 1.23, 32, 31, 0.68, 1.34, 22, 21, 0.59, 2.45, 52, 51, 0.67, 2.63, 42, 41, 0.82, 3.76, 62, 61, 71]
> sco 6  = [0, 6.1, 3.25, 3000, 5555, 1.5, 1.5, 30, 8, 0.8, 1, 32, 21, 0.7, 1.35, 42, 51, 0.6, 1.78, 12, 61, 0.5, 1.13, 62, 11, 0.55, 1.55, 51, 42, 0.76, 1.23, 22, 31, 41]
> sco 7  = [0, 6.5, 1.3, 12000, 250, 1, 0.05, 20, 5, 0.8, 1, 62, 61, 0.7, 2.35, 52, 51, 0.6, 3.78, 42, 41, 0.5, 4.13, 32, 31, 0.55, 5.55, 22, 21, 0.76, 6.23, 12, 11, 71]
> sco 8  = [0, 7.2, 0.95, 12000, 324, 0.6, 0.05, 30, 8, 0.8, 1, 22, 21, 0.7, 2.35, 52, 51, 0.6, 3.78, 62, 61, 0.5, 4.13, 11, 12, 0.55, 5.55, 41, 42, 0.76, 6.23, 31, 32, 72]
> sco 9  = [0, 7.5, 8, 2500, 1000, 2.5, 3.5, 15, 2, 1, 1, 11, 12, 0.86, 2.01, 21, 22, 0.77, 3.02, 31, 32, 0.68, 4.03, 41, 42, 0.59, 5.04, 51, 52, 0.5, 6.05, 61, 62, 73]
> sco 10 = [0, 7.65, 8, 2500, 993, 1.5, 3.5, 15, 2, 1, 1, 12, 11, 0.86, 2.01, 21, 22, 0.77, 3.02, 32, 31, 0.68, 4.03, 41, 42, 0.59, 5.04, 52, 51, 0.5, 6.05, 61, 62, 11]
> sco 11 = [0, 9.35, 0.6, 7000, 850, 0.05, 0.15, 20, 5, 0.8, 1, 22, 11, 0.7, 2.35, 32, 61, 0.6, 3.78, 52, 51, 0.5, 4.13, 42, 21, 0.55, 5.55, 62, 41, 0.76, 6.23, 12, 31, 74]
> sco 12 = [0, 9.55, 6, 9000, 666, 0.15, 3, 20, 5, 0.8, 1, 51, 32, 0.7, 2.35, 31, 12, 0.6, 3.78, 41, 42, 0.5, 4.13, 11, 22, 0.55, 5.55, 61, 62, 0.76, 6.23, 21, 32, 72]
> sco 13 = [0, 10.66, 1.45, 3000, 6200, 1.2, 0.05, 30, 8, 0.8, 1, 22, 21, 0.7, 1.35, 52, 51, 0.6, 1.78, 62, 61, 0.5, 1.13, 12, 11, 0.55, 1.55, 41, 42, 0.76, 1.23, 32, 31, 73]
> sco 14 = [0, 11.87, 1.5, 6000, 1050, 0.1, 1.05, 30, 8, 0.8, 1, 22, 21, 0.7, 1.35, 52, 51, 0.6, 1.78, 62, 61, 0.5, 1.13, 12, 11, 0.55, 1.55, 41, 42, 0.76, 1.23, 32, 31, 74]
> sco 15 = [0, 12, 3.85, 5000, 7733, 0.05, 2.05, 30, 8, 0.8, 1, 22, 21, 0.7, 1.35, 52, 51, 0.6, 1.78, 62, 61, 0.5, 1.13, 12, 11, 0.55, 1.55, 41, 42, 0.76, 1.23, 32, 31, 72]
> sco 16 = [0, 12.05, 4.75, 3000, 9987, 1, 2, 30, 8, 0.8, 1, 22, 21, 0.7, 1.35, 52, 51, 0.6, 1.78, 62, 61, 0.5, 1.13, 12, 11, 0.55, 1.55, 41, 42, 0.76, 1.23, 32, 31, 41]

> scos = map (\x -> (sco x)) [1..16]

> dur1104 (p1:p2:p3:_) = (toRational (p3/2.0))
> rest1104 (p1:p2:_)   = (toRational (p2/2.0))

> note1104 s = (addPFields s (c 3 (dur1104 s)))

> notes = map (\x -> ((Prim (Rest (rest1104 x))) :+: (note1104 x))) scos

> final1104 = foldr (:=:) (Prim (Rest 0)) notes

> record1104 = uncurry (outFile "1104.wav") (renderSF (instrument myInstr1104 final1104) myInstrMap1104)

> main = record1104

------------------------------------------------------------------------------------------------
End of file.
------------------------------------------------------------------------------------------------