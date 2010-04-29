> module FurElise where

> import Euterpea

> rh1 = line [e 6 en, ds 6 en, e 6 en, ds 6 en, e 6 en, b 5 en, 
>             d 6 en, c 6 en, a 5 qn] 
> rh2 = line [c 5 en, e 5 en, a 5 en, b 5 qn]
> rh3 = line [e 5 en , gs 5 en, b 5 en, c 6 qn]
> rh4 = line [e 5 en] :+: rh1
> rh5 = line [e 5 en, c 6 en, b 5 en, a 5 hn]

> rh  = rh1 :+: rest en :+: rh2 :+: rest en :+: rh3 :+: 
>       rest en :+: rh4 :+: rest en :+: rh2 :+: rest en :+: rh5

> lh1 = line [a 3 en, e 4 en, a 4 en]
> lh2 = line [e 4 en, gs 4 en, b 4 en]
> lh3 = lh1 :+: rest dqn :+: lh2 :+: rest dqn :+: lh1 

> lh  = rest wn :+: lh3 :+: rest dqn :+: rest dhn :+: lh3

> furElise = rh :=: lh 
