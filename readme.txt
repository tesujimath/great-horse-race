The Great Horse Race, copyright (c) 2005-2023 Simon J. Guest.
See license.txt for the license.


USAGE:

This is a program to illustrate probability in a graphic way.  There
are currently two races supported: 

1. The classic race, with horses numbered 2 to 12, in which 2 dice are
rolled for each move.

2. The "mystery" race, which is about experimental probability;  the
point is to estimate the probability that each horse takes a step
at each roll.  (The actual probabilities are available on the about dialog.)

To run it, you need the wxHaskell DLL, which is included.  This is an
unmodified version of the library which you can download from the
wxHaskell site, where you can get the source code.  It is included
here for convenience only.


SOURCE CODE:

GreatHorseRace.hs is the source code, in the Haskell programming
language.  In case it's new to you, Haskell is a rather wonderful
programming language, which you can learn a little more about here :-
http://www.haskell.org/tutorial

To build it from source, you need :-

1. Glasgow Haskell Compiler, http://www.haskell.org/ghc/

2. wxHaskell, http://wxhaskell.sourceforge.net/

The source code builds and runs on both Linux and Windows.
Linux is my preferred development platform, and for some reason, the
program runs faster under Linux.  (Races of length 1000 are a little
slow under Windows;  perhaps you should switch to Linux ;-)


sjg 11/1/05
