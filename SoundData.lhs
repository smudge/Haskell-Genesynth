--------------------------------------------------------------------------------
Author:   Nathan Griffith
Contact:  github.com/smudge
          twitter.com/smudgethefirst
Yale University
CPSC 490 (Senior Project): Spring 2010
--------------------------------------------------------------------------------
SoundData module:
  Declarations for representing sound data in haskell
--------------------------------------------------------------------------------

> module SoundData where

+++++++++++++++++
Type Declarations
+++++++++++++++++
SoundData comes in 3 forms: SoundData, BasicSoundData, and ComplexSoundData.

SoundData is the "normal" type, and consists of a list of Partials, each of which has
a frequency value and an envelope.

ComplexSoundData consists of a list of ComplexPartials, each of which consists of
an envelope and a list of (Time,Frequency) pairs. (Allowing Frequency to change over time)

BasicSoundData consists of a list of BasicPartials, each of which consists only of 
a frequency value and an amplitude value.

> --A SoundData is a list of SoundPartials
> type SoundData = [SoundPartial]
> type BasicSoundData = [BasicPartial]
> type ComplexSoundData = [ComplexPartial]

+++++++++++++++++++
The "Root" types
+++++++++++++++++++

> --The unit generator. For now, Sine is the only variant
> data SoundRoot = Sine Freq Amplitude --Amplitude only applies to BasicSoundData
> instance Show SoundRoot where
>   show (Sine f a) = "("++(show f)++","++(show a)++")"
> instance Eq SoundRoot where
>   Sine f1 a1 == Sine f2 a2 = (f1 == f2)&&(a1 == a2)


> --This allows the pitch to change over time
> type FreqRoot = [(Time,Freq)]

+++++++++++++++++++
The Envelope Data
+++++++++++++++++++

> --Envelope with Amplitude at time Zero followed by array of (Time, Amplitude) pairs
> data Envelope = LinSeg [Amplitude] [Time] --Linear
>               | ExpSeg [Amplitude] [Time] --Exponential
> instance Show Envelope where
>   show (LinSeg a t) = "("++(show a)++","++(show t)++")"
> instance Eq Envelope where
>   (LinSeg a1 t1) == (LinSeg a2 t2) = (a1 == a2)&&(t1 == t2)

+++++++++++++++++++
SoundPartial Data
+++++++++++++++++++

> --A SoundPartial consists of a SoundRoot or FreqRoot and an Envelope
> data SoundPartial = SoundPartial SoundRoot Envelope
> data ComplexPartial = ComplexPartial FreqRoot Envelope
> instance Show SoundPartial where
>   show (SoundPartial f e) = "("++(show f)++","++(show e)++")"
> instance Show ComplexPartial where
>   show (ComplexPartial f e) = "("++(show f)++","++(show e)++")"
> instance Eq SoundPartial where
>   (SoundPartial s1 e1) == (SoundPartial s2 e2) = (s1 == s2)&&(e1 == e2)
> type BasicPartial = SoundRoot

+++++++++++++++++++
The Building-Blocks
+++++++++++++++++++

> -- Frequency, Phase, Amplitude, Duration (all Doubles)
> type Freq = Double
> type Phase = Double --probably not needed, but included anyways
> type Amplitude = Double
> type Time = Double

+++++++++++++++++
Useful Functions
+++++++++++++++++

> -- shorthand for creating a "Normal" frequency value (a SoundRoot without the amplitude)
> s :: Freq -> SoundRoot
> s f = Sine f 0

> -- extract the frequency from a soundPartail
> pFreq :: SoundPartial -> Freq
> pFreq (SoundPartial (Sine f p) env) = f

> -- generate an envelope (linear)
> env :: [Amplitude] -> [SoundData.Time] -> Envelope
> env as ts = LinSeg as ts

> -- generate a sound partial (normal)
> sPart :: SoundRoot -> Envelope -> SoundPartial
> sPart x y = SoundPartial x y

> -- generate a complex partial
> cPart :: FreqRoot -> Envelope -> ComplexPartial
> cPart x y = ComplexPartial x y

> --extract fequency or amplitude form a Sine type (used in the GA)
> getF :: (SoundRoot, t) -> Freq
> getF ((Sine f a),g) = f
> getA :: (SoundRoot, t) -> Amplitude
> getA ((Sine f a),g) = a