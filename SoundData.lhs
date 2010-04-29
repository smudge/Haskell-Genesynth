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

> -- Frequency, Phase, Amplitude, Duration (all Doubles)
> type Freq = Double
> type Phase = Double --probably not needed, but included anyways
> type Amplitude = Double
> type Time = Double

> --The unit generator. For now, Sine is the only variant
> data SoundRoot = Sine Freq Amplitude --Amplitude only applies to BasicSoundData
> instance Show SoundRoot where
>   show (Sine f a) = "("++(show f)++","++(show a)++")"
  
> --This allows the pitch to change over time
> type FreqRoot = [(Time,Freq)]

> --Envelope with Amplitude at time Zero followed by array of (Time, Amplitude) pairs
> data Envelope = LinSeg [Amplitude] [Time] --Linear
>               | ExpSeg [Amplitude] [Time] --Exponential

> --A SoundPartial consists of a SoundRoot or FreqRoot and an Envelope
> data SoundPartial = SoundPartial SoundRoot Envelope
> data ComplexPartial = ComplexPartial FreqRoot Envelope

> --A SoundData is a list of SoundPartials
> type SoundData = [SoundPartial]
> type BasicSoundData = [SoundRoot]
> type ComplexSoundData = [ComplexPartial]

+++++++++++++++++
Useful Functions
+++++++++++++++++

> -- extract the frequency from a soundPartail
> pFreq (SoundPartial (Sine f p) env) = f

> -- generate an envelope (linear)
> env as ts = LinSeg as ts

> -- generate a sound partial (normal)
> sPart x y = SoundPartial x y

> -- generate a complex partial
> cPart x y = ComplexPartial x y

> --extract fequency or amplitude form a Sine type
> getF ((Sine f a),g) = f
> getA ((Sine f a),g) = a