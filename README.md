# SimpleSynth

This is a simple implementation of a synthesizer and a DFT visualizer written in Haskell as my project for the course INF221 - Advanced Functional Programming.

The program has two components

## DFT Visualizer
Performs DFT on the provided wave, either by reading a .wav file, or by various input modes (expression, manual, and randomly generated). Allows for equalizing of frequencies by applying a low or high-pass filter, or by equalizing individual frequencies.

## Synthesizer
Create music and melodies by selecting from various oscillators, set BPM, beat length, amount of notes and notes to create melody. Works in conjunction with the DFT Visualizer and equalizer by reading the generated .wav file.


## Installation
To get started, run the following commands:

```
$ git clone https://github.com/kristiansordal/SimpleSynth
$ cd SimpleSynth/app
$ ghci Main
```

### Dependencies
> NOTE: Before installing any packages, to avoid breaking your GHC installation (as I did several times), make sure you don't `cabal install` any package before checkin whether or not they are already installed by `ghcup`. Check this by running `ghc-pkg list | grep <PACKAGE_NAME>`


Run the following commands to install necessary libraries:

```
cabal install mtl
cabal install matplotlib
cabal install random
cabal install array
cabal install carray
cabal install dsp
cabal install extra
cabal install megaparsec
cabal install parser-combinators
cabal install sdl2-mixer
cabal install data-default-class
cabal install WAVE
cabal install containers
cabal install --lib mtl
cabal install --lib matplotlib
cabal install --lib random
cabal install --lib array
cabal install --lib carray
cabal install --lib dsp
cabal install --lib extra
cabal install --lib megaparsec
cabal install --lib parser-combinators
cabal install --lib sdl2-mixer
cabal install --lib data-default-class
cabal install --lib WAVE
cabal install --lib containers
```


