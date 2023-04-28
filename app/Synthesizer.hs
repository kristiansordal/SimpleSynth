{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Synthesizer where

import Control.Monad.State
import Data.List
import qualified Data.Map as Map
import Sound
import Utils
import Wave

type BPM = Integer

type Beats = Integer

data Oscillator
  = Sinusoid {sample :: [Sample]}
  | Sawtooth {sample :: [Sample]}
  | Triangle {sample :: [Sample]}
  | Square {sample :: [Sample]}
  deriving (Show, Eq)

type SynthState = (BPM, Beats, [Oscillator], [Frequency], [[Sample]], [[Sample]])

sampleRate :: Float
sampleRate = 48000

synthesize :: StateT SynthState IO ()
synthesize = do
  putStrLn' "This is a simple implementation on a synthesizer. It works in the following manner: "
  putStrLn' "1: Specify the tempo / BPM of the soundbyte to be generated"
  putStrLn' "2: Select the amount of beats the notes should last"
  putStrLn' "3: Select the type of oscillator to play the notes"
  putStrLn' "4: Select the amount of notes to be played during these beats"
  putStrLn' "5: Select which notes to be played"
  putStrLn' "6: Go to step 2, the new notes will be appended."
  putStrLn' "7: Finished! The soundbyte generated will now be played"
  putStrLn' ""
  putStr' "Select BPM: "
  bpm <- liftIO getLine
  put (read bpm, 0, [], [], [], [])
  synthLoop

initSynthLoop :: StateT SynthState IO ()
initSynthLoop = do
  putStrLn' "Continue adding to the melody? (y / n)"
  putStr' "$ "
  ans <- liftIO getLine
  case ans of
    "y" -> synthLoop
    "n" -> do
      (bpm, beats, osc, notes, samples, noteSamples) <- get
      putStrLn' "Goodbye"
      put (bpm, beats, osc, notes, samples, noteSamples)
    _ -> do
      putStrLn' "Wrong input, please select either (y / n)"
      initSynthLoop

synthLoop :: StateT SynthState IO ()
synthLoop = do
  putStrLn' "Enter the amount of beats"
  putStr' "$ "
  beats <- liftIO getLine
  putStrLn' "Enter the number of oscillators"
  putStr' "$ "
  noOscs <- liftIO getLine
  modify (\(bpm, _, osc, nts, samps, ns) -> (bpm, read beats, osc, nts, samps, ns))
  getOscillators (read noOscs)
  putStrLn' "Select amount of notes to be played"
  putStr' "$ "
  noNotes <- liftIO getLine
  getNotes (read noNotes)
  generateOscillatorSamples

getOscillators :: Int -> StateT SynthState IO ()
getOscillators 0 = return ()
getOscillators x = do
  (bpm, beats, oscillators, notes, samples, noteSamples) <- get
  putStrLn' "Please select oscillator type"
  putStrLn' "1: Sinusoid"
  putStrLn' "2: Sawtooth"
  putStrLn' "3: Triangle"
  putStrLn' "4: Square"
  putStr' "$ "
  osc <- liftIO getLine
  case osc of
    "1" -> do
      put (bpm, beats, nub $ oscillators ++ [Sinusoid []], notes, samples, noteSamples)
    "2" -> do
      put (bpm, beats, nub $ oscillators ++ [Sawtooth []], notes, samples, noteSamples)
    "3" -> do
      put (bpm, beats, nub $ oscillators ++ [Triangle []], notes, samples, noteSamples)
    "4" -> do
      put (bpm, beats, nub $ oscillators ++ [Square []], notes, samples, noteSamples)
    _ -> do
      liftIO $ putStrLn "Please select a number 1-4"
      getOscillators x
  getOscillators (x - 1)

getNotes :: Int -> StateT SynthState IO ()
getNotes 0 = return ()
getNotes x = do
  putStrLn' "Pick a note"
  putStr' "$ "
  note <- liftIO getLine
  case Map.lookup note notes of
    Just n -> do
      (bpm, beats, oscillators, notes, samples, noteSamples) <- get
      put (bpm, beats, oscillators, notes ++ [n], samples, noteSamples)
      getNotes (x - 1)
    Nothing -> do
      putStrLn' "Invalid note, try again."
      getNotes x

generateOscillatorSamples :: StateT SynthState IO ()
generateOscillatorSamples = do
  (bpm, beats, oscillators, notes, samples, noteSamples) <- get
  if null notes
    then do
      putStrLn' $ show $ length noteSamples
      put (bpm, beats, [], notes, samples ++ [interference noteSamples], [])
      initSynthLoop
    else do
      let start = getStart samples
          oscWithSample = map (oscillatorSample start bpm beats (head notes)) oscillators
          interferenceOscillators = interference $ map sample oscWithSample
      if null noteSamples
        then do
          put (bpm, beats, oscillators, drop 1 notes, samples, [interferenceOscillators])
        else do
          put (bpm, beats, oscillators, drop 1 notes, samples, interferenceOscillators : noteSamples)
      generateOscillatorSamples

oscillatorSample :: Float -> BPM -> Beats -> Frequency -> Oscillator -> Oscillator
oscillatorSample start bpm beats freq (Sinusoid _) =
  Sinusoid
    ( sinusoidWaveSample
        start
        freq
        ((60 / fromIntegral bpm) * fromIntegral beats)
        sampleRate
    )
oscillatorSample start bpm beats freq (Sawtooth _) =
  Sawtooth
    ( sawtoothWaveSample
        start
        freq
        ((60 / fromIntegral bpm) * fromIntegral beats)
        sampleRate
    )
oscillatorSample start bpm beats freq (Triangle _) =
  Triangle
    ( triangleWaveSample
        start
        freq
        ((60 / fromIntegral bpm) * fromIntegral beats)
        sampleRate
    )
oscillatorSample start bpm beats freq (Square _) =
  Square
    ( squareWaveSample
        start
        freq
        ((60 / fromIntegral bpm) * fromIntegral beats)
        sampleRate
    )

getStart :: [[Sample]] -> Float
getStart [] = 0
getStart [x] = fst $ last x
getStart xs = fst $ last $ last xs

playSynth :: IO ()
playSynth = do
  (_, _, _, _, samples, _) <- execStateT synthesize (0, 0, [], [], [], [])
  putStrLn "Save file as:"
  putStr "$ "
  fileName <- getLine
  sound <- generateSound (concat samples) (maxBound `div` 10)
  writeWavFile sound fileName
  playSound ("wavfiles/" ++ fileName)
