------------------------------------------------------------------------------------------------
Nathan Griffith
Homework 6:	Physical Modeling
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
> import FurElise


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

> vibrato :: Double -> Double -> AudSF Double Double
> vibrato f d = 
>     proc freq -> do
>       mod <- oscil f1 0 -< f
>       oscil f1 0 -< (freq + d * mod) --shouldn't need normalization

> theFlute :: InstrumentName
> theFlute = Custom "A Flute"
> fluteInstrMap :: InstrMap (AudSF () Double)
> fluteInstrMap = [(theFlute, flute)]

> q = [5]

> f1 = gen10 8192 [1]	
> f2 = gen10 4096 [1, 0.5, 0.333, 0.25, 0.2, 0.166, 0.142, 0.125, 
>                  0.111, 0.1, 0.09, 0.083, 0.076, 0.071, 0.066, 0.062]

> frereJacques =
>   let part1 = (c 5 (1/8)) :+: (d 5 (1/8)) :+: (e 5 (1/8)) :+: (c 5 (1/8))
>       part2 = (e 5 (1/8)) :+: (f 5 (1/8)) :+: (g 5 (1/4))
>       part3 = (g 5 (1/16)) :+: (a 5 (1/16)) :+: (g 5 (1/16)) :+: (f 5 (1/16)) :+: (e 5 (1/8)) :+: (c 5 (1/8))
>       part4 = (c 5 (1/8)) :+: (g 4 (1/8)) :+: (c 5 (1/4)) 
>       dub a = a :+: a
>   in (dub part1 :+: dub part2 :+: dub part3 :+: dub part4)


> dcBlocker :: Double -> AudSF Double Double
> dcBlocker a = proc ain -> do
>   rec let aout = ain - ain1 + a * aout1
>       ain1  <- init 0 -< ain
>       aout1 <- init 0 -< aout
>   returnA -< aout

> addPFields :: [Double] -> Music Pitch -> Music (Pitch, [NoteAttribute])
> addPFields pf = mMap (\p -> (p, [PFields pf]))

> smallRmRvb g (g5,t5) (g4,t4) (g3,t3) (g2,t2) (g1,t1) =
>   proc ain -> do
>   rec a1  <- butterlp                      -< (ain,6000)
>       mid <- dnapf (g3,t3) (g2,t2) (g1,t1) -< a1 + g*fbk
>       fbk <- init 0 <<< butterbp           -< (end*0.5,1600,800)
>       end <- snapf (g5,t5) (g4,t4)         -< mid
>   returnA -< mid*0.5 + end*0.5

> instr104 :: AudSF () Double
> instr104 = proc _ -> do
>              pluck f2 440 SimpleAveraging -< 440

> constA :: Clock p => a -> Signal p () a
> constA = arr.const

------------------------------------------------------------------------------------------------
Start of problems:
------------------------------------------------------------------------------------------------

************************************************************************************************
Create a physical model of a bongo drum using waveguides such that:
	The head of the drum (i.e. the surface membrane) is modeled as a three-node system (instead 
  of four nodes as described in Chapter 19 of TcB).  Two of the nodes represent points near the 
  rim (i.e. edge) of the drum head, where the membrane is taut, because of the proximity to the 
  rim.  The third node represents the middle of the drum head, where the membrane is less taut.  
  Each node should be connected to each of the others via bi-directional waveguides, but the ones 
  between the rim-nodes and the center-node should be different than the one between the two 
  rim-nodes.
	The body of the drum should be viewed as a resonator, and thus should likewise have a waveguide 
  associated with it (unlike the uni-directional waveguide used in Chapter 19).  It should connect 
  only to the center-node on the drum head.
	The excitation from the drumstick can be a triangular impulse as used in Chapter 19.
Note that including the body of the drum, we actually have a four-node waveguide system, which looks 
something like this:

See if you can get this to work.  You may wish to have parameters to control various aspects of 
each kind of bi-directional waveguide above -- red, green, and yellow.  You may inject the drum 
stick excitation at the center-node of the drum head, but for extra credit, consider injecting 
it at different places, getting (hopefully) different sounds.
************************************************************************************************

> --I DID WHAT I COULD HERE -- SEE COMMENTS FOR EXPLANATIONS
> --IT STILL ONLY SOUNDS VAGUELY LIKE A DRUM...........

> bongo :: Instr (AudSF () Double)
> bongo dur pch vol _ =
>     let stick_hit = 0
>         ifqc = 45
>         itube = 2
>         ifdbck2 = 3
>         bodyres = 0
>     in proc _ -> do
>          --INITIALIZE THE DELAY LINE WITH DRUMSTICK NOISE--
>          ashape <- linseg [0, -1/2, 1/2, 0, 0] [1/ifqc/8, 1/ifqc/4, 1/ifqc/8, 1-1/ifqc] -< ()
>          stick_hit <- tone -< (ashape, 1000)
>          rec   
>            --FILTER THE DELAYED SIGNAL AND FEEDBACK INTO THE DELAY--
>            ablock <- init 0 <<< dcBlocker 0.995 -< -(alineba+alineca)/2 --drum edge 2
>            bblock <- init 0 <<< dcBlocker 0.995 -< -(alineab+alinecb)/2 --drum edge 1
>            cblock <- init 0 <<< dcBlocker 0.995 -< -(alinebc+alineac+alinedc)/3 --center of drum head
>            dblock <- init 0 <<< dcBlocker 0.995 -< -(alinecd) --body tube
>            --DELAY LINES--
>            --lines to/from node a and node b (edges) --
>            alineab <- delay (1.0/ifqc) <<< butterbp -< (ablock, 260, 100) --because they are more taught, 
>            alineba <- delay (1.0/ifqc) <<< butterbp -< (bblock, 260, 100) --they only resonate closer to a specific frequency (middle C -- 260Hz)
>            --lines to/from node c and nodes a and b --
>            alinebc <- delay (2.0/ifqc) -< bblock
>            alineac <- delay (2.0/ifqc) -< ablock
>            alineca <- delay (2.0/ifqc) -< cblock+stick_hit --the stick hit originates from node C (move "+stick_hit" around to change!!!)
>            alinecb <- delay (2.0/ifqc) -< cblock+stick_hit --the stick hit originates from node C
>            --lines to/from node c and node d (body chamber) --
>            alinedc <- delay (3.0/ifqc) <<< butterhp -< (dblock, 400) --lower frequencies escape faster
>            alinecd <- delay (3.0/ifqc) -< cblock+stick_hit
>          returnA -< (ablock+bblock+cblock+dblock)

> myBongo :: InstrumentName
> myBongo = Custom "A Waveguide Bongo"
> myInstrMap :: InstrMap (AudSF () Double)
> myInstrMap = [(myBongo, bongo)]

> s = []

> beat = ((c 5 (1/8)):+:(rest (1/8)):+:(c 5 (1/8)):+:(rest (1/8)))

> recordBongo = uncurry (outFile "bongo.wav") (renderSF (instrument myBongo (addPFields s (beat :+: beat))) myInstrMap)


************************************************************************************************
Implement either a medium-room reverberator, or a large-room reverberator, as defined in Chapter 
24 of TcB (Implementing the Gardner Reverbs in Csound).  However, do not simply transcribe the 
csound code -- you should use the recursive methods described in class.  Test your reverberator 
on some music.
************************************************************************************************

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
>          fbk <- delay 0.108 <<< init 0 <<< butterbp -< (a3 * 0.4, 1000, 500)
>          let aout = a1 * 0.5 + a2 * 0.5 + a3 * 0.5
>        returnA -< aout


***********
Test with basic instrument:
***********

> main = outFile "reverb.wav" 3.0 (mediumReverb <<< instr104)

> --compare this to the small room reverb
> smallReverbTest = smallRmRvb 0.4 (0.3, 30) (0.08,66) (0.3, 8.3) (0.25,22) (0.15,35)
> mainSmall = outFile "small_reverb.wav" 3.0 (smallReverbTest <<< instr104)



***************************
test this with the flute:
***************************

> recordFlute = uncurry (outFile "flute.wav") (head (map (\(x,y) -> (x+2,mediumReverb <<< y)) [(renderSF (instrument theFlute (addPFields q frereJacques)) fluteInstrMap)]))

> --TEST THIS AGAINST SMALL ROOM REVERB:
> recordFluteSmall = uncurry (outFile "flute_small.wav") (head (map (\(x,y) -> (x+2,smallReverbTest <<< y)) [(renderSF (instrument theFlute (addPFields q frereJacques)) fluteInstrMap)]))