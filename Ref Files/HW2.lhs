Nathan Griffith
Homework #2
Sound and Signals

Due: Friday Oct 2 at 11:59pm.

---------------------------------------------------------------------------
1. For each of the following, say whether it is a longitudinal wave or a transverse:
---------------------------------------------------------------------------
A vibrating violin string:
-transverse (the string itself -- the vibrating air around it is longitudinal)
Stop-and-go traffic on a highway:
-longitudinal
"The wave" in a crowd at a stadium:
-transverse
"Water hammer" in the plumbing of your house:
-longitudinal
The wave caused by a stone falling in a pond:
-visualized as a transverse wave (though in actuality it's a combination of both that occurs)
A radio wave:
-transverse

---------------------------------------------------------------------------
2. You see a lightening strike, and 5 seconds later you hear the thunder.  How far away is the lightening?
---------------------------------------------------------------------------
1125 feet/second * 5 seconds =
5625 feet, 1.065 miles

(assuming that the approximately 5.719x10^-6 seconds it took the light to reach you is negligible.)

---------------------------------------------------------------------------
3. You clap your hands in a canyon, and 2 seconds later you hear an echo.  How far away is the canyon wall?
---------------------------------------------------------------------------
We can assume it took 1 second to reach the canyon wall and 1 second to get back, since sound travels at a constant rate through like materials.

1125 feet/second * 1 second = 1125 feet.

---------------------------------------------------------------------------
4. By what factor must one increase the RMS level of a signal to yield a 10 dB increase in sound level?
---------------------------------------------------------------------------
Accepted reference level for human ear R = 10^-12
SdB = 10log[10]( S RMS / R)

So, we know that SdB will need to be 10. This means we must solve for S (which is measured in RMS)
Since R is constant, we only need to determine by what factor S/R increases, so let's call this value x.

0 = 10log[10] ( x )
0 = log[10]( x )
0 = log(x)
x = 1

10 = 10log[10] ( x )
1 = log[10]( x )
1 = log(x) / log(10)
log(x) = log(10)
x = 10

20 = 10log[10] ( x )
2 = log[10]( x )
2 = log(x) / log(10)
log(x) = 2log(10)
log(x) = log(10^2)
x = 10^2 = 100

We see a pattern here.

As dB increases by incrments of 10, x (and thus the RMS of the signal) increases by a factor of 10.

To prove this for the general case:

y = 10log[10]( x )
y/10 = log[10]( x ) = log(x) / log(10)
log(x) = y/10log(10)
log(x) = log(10^(y/10))
x = 10^(y/10)

We now see that increasing y by 10 will increase x by a factor of 10^1, or 10.


---------------------------------------------------------------------------
5. What is the RMS value of a square wave that varies between -1 and +1 units?  How about a sawtooth wave 
that varies over the same range?
---------------------------------------------------------------------------
Square wave: 
y =  1 ((ft)%1) < 0.5
    -1 ((ft)%1) > 0.5
    
To solve for this, we can take the two values 1 and -1 and enter them into the equation:
xRMS = sqrt((x1^2 + x2^2 + ... + xn^2)/n)
xRRMS = sqrt((1^2 + (-1)^2)/2)
= sqrt((1 + 1)/2)
= sqrt(1)
= 1


Sawtooth wave: 
y = 0.5 - 2((ft)%1)


If we input this into our function we get:

lim T->infinity of ( sqrt ( 1/2T * integral from -T to T ([0.5 - 2((ft)%1)]^2 * dt) ) )

Solve and get:
1/squrt(3)

---------------------------------------------------------------------------
6. A dog can hear in the range 60-45,000 Hz, and a bat 2,000-110,000 Hz.  What are the corresponding 
dynamic ranges for these two animals, and how does it compare to that of humans?
---------------------------------------------------------------------------
For these calculations, x dB = 10log[10]( top of range / bottom of range )

humans:
20,000Hz/20Hz = 1000 = 30dB

dogs:
45,000Hz/60Hz = 750 ~= 28.75 dB

bats:
110,000Hz/2,000Hz = 55 ~= 17.4 dB

Both dynamic ranges are smaller than that of humans. 
One would think that for dogs and bats it would be much 
larger, since they can hear much higher frequencies,
but since most humans can hear down to 20Hz, our total 
range from bottom to top increases by a larger factor.

---------------------------------------------------------------------------
7. What is the maximum number of audible overtones in a note whose fundamental frequency is 100 Hz?  
500 Hz? 1500 Hz?  5 kHz?
---------------------------------------------------------------------------
Let's assume the overtones will all be in multiples above the fundamental frequency, since we are playing a single note.
So if you take 100Hz as the fundamental freq, there are 20,000Hz/100Hz possible overtones.

If the returned number of overtones is not an integer, the number of overtones is the floor of that value.

so,
20,000Hz/100Hz = 200 overtones.
20,000Hz/500Hz = 40 overtones.
20,000Hz/1500Hz = floor(13.333) = 13 overtones.
20,000Hz/5000Hz = 4 overtones.

---------------------------------------------------------------------------
8. Devise a formula for the frequency of the reproduced signal r given an input signal f and a sample rate s.
---------------------------------------------------------------------------
If we remember the table provided in 16.2.1, we can convert it to a formula that approximates an exact formula:
r = s >= 2f, f
    2f > s >= f, from f to 0
    f > s > 2/3f, from 0 to f
    2/3f > s > 1/2f, from f to 0
    1/2f > s > 2/5f, from 0 to f
    ...
    
We see that for 0 < s < 2f this is actually a wave function which has an amplitude of f and is offset vertically by f/2,
so that it moves between f and 0. The period is not constant, and instead seems to be based on some sort of 2/n function, where n starts at 1.

So, we can visualize this as something like:
r = .5f + .5fcos(pi*(2/s)*2f )

Where s > 0 and s <= 2f. At s >= 2f, r = f

---------------------------------------------------------------------------
9. How much memory is needed to record 3 minutes of stereo sound using 16-bit samples taken at a rate of 44.1 kHz?
---------------------------------------------------------------------------
44,100 samples / second * 16 bits / sample * 180 seconds = 1.27 * 10^8 bits
~= 15.88 MB

---------------------------------------------------------------------------
10. If I want the best possible sound, how large should the table be using fixed-waveform table-lookup 
synthesis, in order to cover the audible frequency range?
---------------------------------------------------------------------------
We want to be able to accurately cover all audible frequencies, which means we want it wide enough
to store a single period at 20Hz, but also be able to store 20,000Hz.

There are multiple methods of storing and changing frequencies. From what I've read, the standard way
of changing frequencies is by skipping certain values and reading through the wavetable at a different rate.
(Instead of changing the actual values in the wavetable.) This method is used in many oscillators.

This means the number of samples depends on the sampling frequency. If the sampling frequency is 100,000Hz
and the table contains 1000 entries, the outputted frequency is 100,000/1000 = 100Hz.

For this problem, we need to know the sampling freqency and the increment. Then we can use this equation:

tableLength = increment x samplingFrequency / desiredFrequency

So, for example, if the samplingFrequency is 44,100Hz and the increment is 1:

tableLength = 44,100Hz / 20Hz
= 2,205

To get a frequency of 20Hz, it would have to just read through 44,100/20,000 individual samples.

Due to rounding issues, you may need to increase the resolution even further (so that at 20,000Hz you read 2 accurate samples). 

This would require increasing the samplingFrequency.

However, mathematically, 44,1000 is greater than 2x20,000, and should therefore be okay.

---------------------------------------------------------------------------
11. The Doppler effect occurs when a sound source is in motion.  For example, as a police car moves 
toward you its siren sounds higher than it really is, and as it goes past you, it gets lower.  How 
fast would a police car have to go to change a siren whose frequency is the same as concert A, to a 
pitch an octave higher? (i.e. twice the frequency)  At that speed, what frequency would we hear after 
the police car passes us?
---------------------------------------------------------------------------

percievedFrequency = (speed of sound / (speed of sound + velocityOfPoliceCar)) * emittedFrequency

Concert A = 440Hz
Octave above = 880Hz
Speed of sound = 340.29 m / s

880Hz = ( 340.29 / (340.29 + vP)) * 440Hz
2 = 340.29 / (340.29 + vP)
340.29 + vP = 340.29 / 2
vP = 340.29 / 2 - 340.29
vP =  -170.145 meters / second

(This makes sense, because the police car is moving TOWARDS you, so relative to you it is -170.145 m/s)

SO the police car must move at 170.145 meters / second

After it passes us it is moving at +170.145 m/s, we hear frequency f:
f = ( 340.29 / (340.29 + 170.145)) * 440Hz
f = 293.3333... Hz
or about 293 Hz

Which is the D below concert A

---------------------------------------------------------------------------