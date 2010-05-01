> {-# LANGUAGE Arrows #-}

> module SoundDataRender where

> import Euterpea hiding (line,delay)
> import Euterpea.Audio.Basics
> import Euterpea.Audio.CSoundGenerators
> import Euterpea.Audio.Types
> import Euterpea.Audio.IO
> import Euterpea.Audio.Render
> import Prelude hiding (init)
> import Control.Arrow hiding (returnA)
> import Control.CCA.Types
> import SoundData


> --Useful functions (aliases, shorthand)
> s f = Sine f 0
> type AudSF a b = Signal AudRate a b
> type CtrSF a b = Signal CtrRate a b

> --pre-defined sine tables / generators / oscilators
> sineTable = compSine1 16384 [1]
> f1 = gen10 8192 [1]	
> oscSine = oscil sineTable 0

> --single sine oscillator (for rendering a single frequency)
> sine :: Double -> AudSF () Double
> sine a = 
>   proc _ -> do
>     oscSine -< a

> --HELPERS for the vaious sine Additive Synthesis functions
> amps_ (LinSeg a b) = a
> durs_ (LinSeg a b) = hDurs b --change time values to duration valuess
> hDurs x = hRec x [] 0
>   where hRec [] durs prevs = durs
>         hRec times durs prevs = hRec (tail times) (durs ++ [(head times)-prevs]) (head times)
> el es n = if ((length es) > n) then (es !! n) else (env [1.0] [])
> sl xs n = if ((length xs) > n) then (xs !! n) else 0

> --Performs Additive Synthesis on a basic SoundData value (an Array of SoundRoots)
> --   *Note the hard limit on the number of frequencies
> --   *sinesBasic assumes a fixed frequency and fixed amplitude
> sinesBasic :: BasicSoundData -> AudSF () Double
> sinesBasic sb =
>   let freqs = map (\(Sine f a) -> f) sb
>       ampls = map (\(Sine f a) -> a) sb
>       fs x = if ((length sb) > x) then (freqs !! x) else 0
>       as x = if ((length sb) > x) then (ampls !! x) else 0
>   in proc _ -> do
>     x0  <- oscSine -< fs 0            --PARTIALS
>     x1  <- oscSine -< fs 1
>     x2  <- oscSine -< fs 2
>     x3  <- oscSine -< fs 3
>     x4  <- oscSine -< fs 4
>     x5  <- oscSine -< fs 5
>     x6  <- oscSine -< fs 6
>     x7  <- oscSine -< fs 7
>     x8  <- oscSine -< fs 8
>     x9  <- oscSine -< fs 9
>     x10 <- oscSine -< fs 10
>     x11 <- oscSine -< fs 11
>     x12 <- oscSine -< fs 12
>     x13 <- oscSine -< fs 13
>     x14 <- oscSine -< fs 14
>     x15 <- oscSine -< fs 15
>     let sum = x0*(as 0)+x1*(as 1)+x2*(as 2)+x3*(as 3)+x4*(as 4)+x5*(as 5)+x6*(as 6)+x7*(as 7)+x8*(as 8)+x9*(as 9)+x10*(as 10)+x11*(as 11)+x12*(as 12)+x13*(as 13)+x14*(as 14)+x15*(as 15)
>     returnA -< sum/(fromIntegral (min 15 (length sb)))


> --Performs Additive Synthesis on a SoundData value (an Array of SoundPartials)
> --   *Note the hard limit on the number of SoundPartials
> --   *sinesNormal assumes a fixed frequency
> sinesNormal :: SoundData -> AudSF () Double
> sinesNormal as =
>   let sr = map (\(SoundPartial a b) -> a) as
>       es = map (\(SoundPartial a b) -> b) as
>       xs = map (\(Sine a b) -> a) sr
>       amps x = amps_ (el es x)
>       durs x = durs_ (el es x)
>   in proc _ -> do
>     e0  <- linseg (amps 0)  (durs 0)  -< () --ENVELOPES
>     e1  <- linseg (amps 1)  (durs 1)  -< ()
>     e2  <- linseg (amps 2)  (durs 2)  -< ()
>     e3  <- linseg (amps 3)  (durs 3)  -< ()
>     e4  <- linseg (amps 4)  (durs 4)  -< ()
>     e5  <- linseg (amps 5)  (durs 5)  -< ()
>     e6  <- linseg (amps 6)  (durs 6)  -< ()
>     e7  <- linseg (amps 7)  (durs 7)  -< ()
>     e8  <- linseg (amps 8)  (durs 8)  -< ()
>     e9  <- linseg (amps 9)  (durs 9)  -< ()
>     e10 <- linseg (amps 10) (durs 10) -< ()
>     e11 <- linseg (amps 11) (durs 11) -< ()
>     e12 <- linseg (amps 12) (durs 12) -< ()
>     e13 <- linseg (amps 13) (durs 13) -< ()
>     e14 <- linseg (amps 14) (durs 14) -< ()
>     e15 <- linseg (amps 15) (durs 15) -< ()
>     x0  <- oscSine -< sl xs 0            --PARTIALS
>     x1  <- oscSine -< sl xs 1
>     x2  <- oscSine -< sl xs 2
>     x3  <- oscSine -< sl xs 3
>     x4  <- oscSine -< sl xs 4
>     x5  <- oscSine -< sl xs 5
>     x6  <- oscSine -< sl xs 6
>     x7  <- oscSine -< sl xs 7
>     x8  <- oscSine -< sl xs 8
>     x9  <- oscSine -< sl xs 9
>     x10 <- oscSine -< sl xs 10
>     x11 <- oscSine -< sl xs 11
>     x12 <- oscSine -< sl xs 12
>     x13 <- oscSine -< sl xs 13
>     x14 <- oscSine -< sl xs 14
>     x15 <- oscSine -< sl xs 15
>     let sum = x0*e0+x1*e1+x2*e2+x3*e3+x4*e4+x5*e5+x6*e6+x7*e7+x8*e8+x9*e9+x10*e10+x11*e11+x12*e12+x13*e13+x14*e14+x15*e15
>     returnA -< sum/(fromIntegral (min 15 (length xs)))

> --Performs Additive Synthesis on a SoundData value (an Array of SoundPartials)
> --   *Note the hard limit on the number of SoundPartials
> --   *sinesNormal assumes a fixed frequency
> sinesComplex :: ComplexSoundData -> AudSF () Double
> sinesComplex inst =
>   let as = take 15 inst --limit to 15!
>       freqRoot = map (\(ComplexPartial a b) -> a) as
>       envelope = map (\(ComplexPartial a b) -> b) as
>       freqs = (map (map snd) freqRoot)
>       freqN n = getN n freqs
>       fdurs = (map (map fst) freqRoot)
>       fdurN n = tail (hDurs (getN n fdurs))
>       getN n lst = if ((length lst) > n) then (lst !! n) else [0.0]
>       amps x = amps_ (el envelope x)
>       durs x = durs_ (el envelope x)
>   in proc _ -> do
>     f0  <- linseg (freqN 0)  (fdurN 0)   -< () --Frequencies
>     f1  <- linseg (freqN 1)  (fdurN 1)   -< ()
>     f2  <- linseg (freqN 2)  (fdurN 2)   -< ()
>     f3  <- linseg (freqN 3)  (fdurN 3)   -< ()
>     f4  <- linseg (freqN 4)  (fdurN 4)   -< ()
>     f5  <- linseg (freqN 5)  (fdurN 5)   -< ()
>     f6  <- linseg (freqN 6)  (fdurN 6)   -< ()
>     f7  <- linseg (freqN 7)  (fdurN 7)   -< ()
>     f8  <- linseg (freqN 8)  (fdurN 8)   -< ()
>     f9  <- linseg (freqN 9)  (fdurN 9)   -< ()
>     f10 <- linseg (freqN 10) (fdurN 10)  -< ()
>     f11 <- linseg (freqN 11) (fdurN 11)  -< ()
>     f12 <- linseg (freqN 12) (fdurN 12)  -< ()
>     f13 <- linseg (freqN 13) (fdurN 13)  -< ()
>     f14 <- linseg (freqN 14) (fdurN 14)  -< ()
>     f15 <- linseg (freqN 15) (fdurN 15)  -< ()
>     e0  <- linseg (amps 0)  (durs 0)  -< () --ENVELOPES
>     e1  <- linseg (amps 1)  (durs 1)  -< ()
>     e2  <- linseg (amps 2)  (durs 2)  -< ()
>     e3  <- linseg (amps 3)  (durs 3)  -< ()
>     e4  <- linseg (amps 4)  (durs 4)  -< ()
>     e5  <- linseg (amps 5)  (durs 5)  -< ()
>     e6  <- linseg (amps 6)  (durs 6)  -< ()
>     e7  <- linseg (amps 7)  (durs 7)  -< ()
>     e8  <- linseg (amps 8)  (durs 8)  -< ()
>     e9  <- linseg (amps 9)  (durs 9)  -< ()
>     e10 <- linseg (amps 10) (durs 10) -< ()
>     e11 <- linseg (amps 11) (durs 11) -< ()
>     e12 <- linseg (amps 12) (durs 12) -< ()
>     e13 <- linseg (amps 13) (durs 13) -< ()
>     e14 <- linseg (amps 14) (durs 14) -< ()
>     e15 <- linseg (amps 15) (durs 15) -< ()
>     x0  <- oscSine -< f0            --PARTIALS
>     x1  <- oscSine -< f1
>     x2  <- oscSine -< f2
>     x3  <- oscSine -< f3
>     x4  <- oscSine -< f4
>     x5  <- oscSine -< f5
>     x6  <- oscSine -< f6
>     x7  <- oscSine -< f7
>     x8  <- oscSine -< f8
>     x9  <- oscSine -< f9
>     x10 <- oscSine -< f10
>     x11 <- oscSine -< f11
>     x12 <- oscSine -< f12
>     x13 <- oscSine -< f13
>     x14 <- oscSine -< f14
>     x15 <- oscSine -< f15
>     let sum = x0*e0+x1*e1+x2*e2+x3*e3+x4*e4+x5*e5+x6*e6+x7*e7+x8*e8+x9*e9+x10*e10+x11*e11+x12*e12+x13*e13+x14*e14+x15*e15
>     returnA -< sum/(fromIntegral (min 15 (length freqs)))


> --TESTING
> testStuff inst n = freqN n
>   where as = take 15 inst --limit to 15!
>         freqRoot = map (\(ComplexPartial a b) -> a) as
>         envelope = map (\(ComplexPartial a b) -> b) as
>         freqs = (map (map snd) freqRoot)
>         freqN n = getN n freqs
>         fdurs = (map (map fst) freqRoot)
>         fdurN n = hDurs (getN n fdurs)
>         getN n lst = if ((length lst) > n) then (lst !! n) else [0.0]
>         amps x = amps_ (el envelope x)
>         durs x = durs_ (el envelope x)

> --RENDER THE SOUND TO A FILE
> renderSoundN name dur dat = outFile name dur (sinesNormal  dat)
> renderSoundC name dur dat = outFile name dur (sinesComplex dat)
> renderSoundB name dur dat = outFile name dur (sinesBasic dat)
