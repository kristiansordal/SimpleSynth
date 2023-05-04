{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Synthesizer where

import Control.Monad.State
import Data.List (nub)
import qualified Data.Map as Map
import Sound (generateSound, repeatSound, writeWavFile)
import Utils
import Wave
  ( Sample,
    interference,
    sawtoothWaveSample,
    sinusoidWaveSample,
    squareWaveSample,
    triangleWaveSample,
  )

type BPM = Integer

type Beats = Float

-- Data type for the available oscillators.
-- They have a list of samples, which is used to generate the interferrence
-- of several oscillators
data Oscillator
  = Sinusoid {sample :: [Sample]}
  | Sawtooth {sample :: [Sample]}
  | Triangle {sample :: [Sample]}
  | Square {sample :: [Sample]}
  deriving (Show, Eq)

-- This is the state of the Synthesizer
-- BPM - The beats per minute of the sound generated
-- Beats - How many beats the current notes being created should span
-- [Oscillator] - Which oscillators are currently being used
-- [Frequency] - The frequencies of the notes for the current span of beats
-- [[Sample]]_1 - The samples of the melody so far
-- [[Sample]]_2 - The samples of the notes in the current span - These are interferred and added to [[Sample]]_1
-- after the list of notes are exhausted
type SynthVars =
  ( BPM,
    Beats,
    [Oscillator],
    [Frequency],
    [[Sample]],
    [[Sample]]
  )

type SynthState = StateT SynthVars IO ()

sampleRate :: Float
sampleRate = 48000

synthesize :: SynthState
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
  bpm <- liftIO $ inputFloat "Select BPM: "
  put (round bpm, 0, [], [], [], [])
  noOscs <- liftIO $ inputFloat "Enter the number of oscillators"
  getOscillators (round noOscs)
  synthLoop

-- Initialize a new synthloop
initSynthLoop :: SynthState
initSynthLoop = do
  putStrLn' "Continue adding to the melody? (y / n)"
  putStr' "$ "
  ans <- liftIO getLine
  case ans of
    "y" -> synthLoop
    "Y" -> synthLoop
    "n" -> do
      (bpm, beats, osc, notes, samples, noteSamples) <- get
      put (bpm, beats, osc, notes, samples, noteSamples)
    "N" -> do
      (bpm, beats, osc, notes, samples, noteSamples) <- get
      put (bpm, beats, osc, notes, samples, noteSamples)
    _ -> do
      putStrLn' "Wrong input, please select either (y / n)"
      initSynthLoop

-- Get the number of beats, oscillators and notes
synthLoop :: SynthState
synthLoop = do
  beats <- liftIO $ inputFloat "Enter the amount of beats"
  modify (\(bpm, _, o, n, ns, nss) -> (bpm, beats, o, n, ns, nss))

  noNotes <- liftIO $ inputFloat "Select amount of notes to be played"
  getNotes (round noNotes)

  generateOscillatorSamples

-- Get the type of oscillator
getOscillators :: Int -> SynthState
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

  -- Duplicate oscillators are removed with nub, as using multiple oscillators of the same type
  -- won't make a difference in sound and hurts performance.
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

-- Gets the frequencies of notes from the lookup table in Utils
getNotes :: Int -> SynthState
getNotes 0 = return ()
getNotes x = do
  putStrLn' "Pick a note"
  putStr' "$ "
  note <- liftIO getLine
  case Map.lookup note notes of
    Just n -> do
      modify (\(bpm, bts, osc, nts, spl, nspl) -> (bpm, bts, osc, nts ++ [n], spl, nspl))
      getNotes (x - 1)
    Nothing -> do
      putStrLn' "Invalid note, try again."
      getNotes x

-- Generates samples for each oscillator and interfers them
generateOscillatorSamples :: SynthState
generateOscillatorSamples = do
  (bpm, beats, oscillators, notes, samples, noteSamples) <- get
  if null notes
    then do
      -- When we have created samples for all the notes, we update the state by
      -- appending the interferrence of these notes to the samples we have.
      -- We also clear the note sample list to prime it for new note samples to be
      -- created
      put (bpm, beats, oscillators, [], samples ++ [interference noteSamples], [])
      initSynthLoop
    else do
      -- As the samples are coordinate based, the start of the new samples need to pick up
      -- where the previous left off
      let start = getStart samples

          -- Generate the samples for each of the oscillators at the given note
          oscWithSample = map (oscillatorSample start bpm beats (head notes)) oscillators

          -- Interfer the samples to create the sound of both oscillators playing the note at the same time
          interferenceOscillators = interference $ map sample oscWithSample

      if null noteSamples
        then do
          put (bpm, beats, oscillators, drop 1 notes, samples, [interferenceOscillators])
          generateOscillatorSamples
        else do
          put (bpm, beats, oscillators, drop 1 notes, samples, interferenceOscillators : noteSamples)
          generateOscillatorSamples

oscillatorSample :: Float -> BPM -> Beats -> Frequency -> Oscillator -> Oscillator
oscillatorSample start bpm beats freq (Sinusoid _) = Sinusoid (sinusoidWaveSample start freq ((60 / fromIntegral bpm) * beats) sampleRate)
oscillatorSample start bpm beats freq (Sawtooth _) = Sawtooth (sawtoothWaveSample start freq ((60 / fromIntegral bpm) * beats) sampleRate)
oscillatorSample start bpm beats freq (Triangle _) = Triangle (triangleWaveSample start freq ((60 / fromIntegral bpm) * beats) sampleRate)
oscillatorSample start bpm beats freq (Square _) = Square (squareWaveSample start freq ((60 / fromIntegral bpm) * beats) sampleRate)

playSynth :: IO ()
playSynth = do
  (_, _, _, _, samples, _) <- execStateT synthesize (0, 0, [], [], [], [])
  putStrLn "Save file as:"
  putStr "$ "
  fileName <- getLine
  sound <- generateSound (concat samples) (maxBound `div` 10) (round sampleRate)
  writeWavFile sound fileName
  repeatSound ("wavfiles/" ++ fileName)
