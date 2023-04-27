{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Synthesizer where

import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as Map
import Parser
import Sound
import Utils
import Wave

type Frequency = Float

type BPM = Integer

type Beats = Integer

type Note = String

data Oscillator
  = Sinusoid {sample :: [Sample]}
  | Sawtooth {sample :: [Sample]}
  | Triangle {sample :: [Sample]}
  | Square {sample :: [Sample]}
  deriving (Show)

type SynthState = (BPM, Beats, [Oscillator], [Frequency], [[Sample]])

sampleRate :: Float
sampleRate = 48000

notes :: Map Note Frequency
notes =
  Map.fromList
    [ ("C0", 6.35),
      ("C#0", 17.32),
      ("Db0", 1.32),
      ("D0", 8.35),
      ("D#0", 19.45),
      ("Eb0", 1.45),
      ("E0", 0.60),
      ("F0", 1.83),
      ("F#0", 23.12),
      ("Gb0", 3.12),
      ("G0", 4.50),
      ("G#0", 25.96),
      ("Ab0", 5.96),
      ("A0", 7.50),
      ("A#0", 29.14),
      ("Bb0", 9.14),
      ("B0", 0.87),
      ("C1", 2.70),
      ("C#1", 34.65),
      ("Db1", 4.65),
      ("D1", 6.71),
      ("D#1", 38.89),
      ("Eb1", 8.89),
      ("E1", 1.20),
      ("F1", 3.65),
      ("F#1", 46.25),
      ("Gb1", 6.25),
      ("G1", 9.00),
      ("G#1", 51.91),
      ("Ab1", 1.91),
      ("A1", 5.00),
      ("A#1", 58.27),
      ("Bb1", 8.27),
      ("B1", 1.74),
      ("C2", 5.41),
      ("C#2", 69.30),
      ("Db2", 9.30),
      ("D2", 3.42),
      ("D#2", 77.78),
      ("Eb2", 7.78),
      ("E2", 2.41),
      ("F2", 7.31),
      ("F#2", 92.50),
      ("Gb2", 2.50),
      ("G2", 8.00),
      ("G#2", 103.83),
      ("Ab2", 13.83),
      ("A2", 10.00),
      ("A#2", 116.54),
      ("Bb2", 16.54),
      ("B2", 23.47),
      ("C3", 30.81),
      ("C#3", 138.59),
      ("Db3", 18.59),
      ("D3", 46.83),
      ("D#3", 155.56),
      ("Eb3", 15.56),
      ("E3", 64.81),
      ("F3", 74.61),
      ("F#3", 185.00),
      ("Gb3", 15.00),
      ("G3", 96.00),
      ("G#3", 207.65),
      ("Ab3", 27.65),
      ("A3", 20.00),
      ("A#3", 233.08),
      ("Bb3", 23.08),
      ("B3", 46.94),
      ("C4", 61.63),
      ("C#4", 277.18),
      ("Db4", 27.18),
      ("D4", 93.66),
      ("D#4", 311.13),
      ("Eb4", 31.13),
      ("E4", 29.63),
      ("F4", 49.23),
      ("F#4", 369.99),
      ("Gb4", 39.99),
      ("G4", 92.00),
      ("G#4", 415.30),
      ("Ab4", 45.30),
      ("A4", 40.00),
      ("A#4", 466.16),
      ("Bb4", 466.16),
      ("B4", 93.88),
      ("C5", 23.25),
      ("C#5", 554.37),
      ("Db5", 554.37),
      ("D5", 87.33),
      ("D#5", 622.25),
      ("Eb5", 622.25),
      ("F5", 98.46),
      ("E5", 59.25),
      ("F#5", 739.99),
      ("Gb5", 739.99),
      ("G5", 83.99),
      ("G#5", 830.61),
      ("Ab5", 830.61),
      ("A5", 80.00),
      ("A#5", 932.33),
      ("Bb5", 932.33),
      ("B5", 87.77),
      ("C6", 046.50),
      ("C#6", 1108.73),
      ("Db6", 1108.73),
      ("D6", 174.66),
      ("D#6", 1244.51),
      ("Eb6", 1244.51),
      ("E6", 318.51),
      ("F6", 396.91),
      ("F#6", 1479.98),
      ("Gb6", 1479.98),
      ("G6", 567.98),
      ("G#6", 1661.22),
      ("Ab6", 1661.22),
      ("A6", 760.00),
      ("A#6", 1864.66),
      ("Bb6", 1864.66),
      ("B6", 975.53),
      ("C7", 093.00),
      ("C#7", 2217.46),
      ("Db7", 2217.46),
      ("D7", 349.32),
      ("D#7", 2489.02),
      ("Eb7", 2489.02),
      ("E7", 637.02),
      ("F7", 793.83),
      ("F#7", 2959.96),
      ("Gb7", 2959.96),
      ("G7", 135.96),
      ("G#7", 3322.44),
      ("Ab7", 3322.44),
      ("A7", 520.00),
      ("A#7", 3729.31),
      ("Bb7", 3729.31),
      ("B7", 951.07),
      ("C8", 186.01),
      ("C#8", 4434.92),
      ("Db8", 4434.92),
      ("D8", 698.63),
      ("D#8", 4978.03),
      ("Eb8", 4978.03),
      ("E8", 274.04),
      ("F8", 587.65),
      ("F#8", 5919.91),
      ("Gb8", 5919.91),
      ("G8", 271.93),
      ("G#8", 6644.88),
      ("Ab8", 6644.88),
      ("A8", 040.00),
      ("A#8", 7458.62),
      ("Bb8", 7458.62),
      ("B8", 902.13)
    ]

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
  put (read bpm, 0, [], [], [])
  synthLoop

initSynthLoop :: StateT SynthState IO ()
initSynthLoop = do
  putStrLn' "Continue adding to the melody? (y / n)"
  putStr' "$ "
  ans <- liftIO getLine
  case ans of
    "y" -> synthLoop
    "n" -> do return ()
    -- (bpm, beats, osc, notes, samples) <- get
    -- putStrLn' "Goodbye"
    -- put (bpm, beats, osc, notes, samples)
    _ -> do
      putStrLn' "Wrong input, please select either (y / n)"
      initSynthLoop

synthLoop :: StateT SynthState IO ()
synthLoop = do
  (bpm, _, _, _, _) <- get
  putStrLn' "Enter the amount of beats"
  putStr' "$ "
  beats <- liftIO getLine
  putStrLn' "Enter the number of oscillators"
  putStr' "$ "
  noOscs <- liftIO getLine
  put (bpm, read beats, [], [], [])
  getOscillators (read noOscs)

getOscillators :: Int -> StateT SynthState IO ()
getOscillators 0 = do
  putStrLn' "Select amount of notes to be played"
  putStr' "$ "
  noNotes <- liftIO getLine
  getNotes (read noNotes)
getOscillators x = do
  (bpm, beats, oscillators, notes, samples) <- get
  putStrLn' "Please select oscillator type"
  putStrLn' "1: Sinusoid"
  putStrLn' "2: Sawtooth"
  putStrLn' "3: Triangle"
  putStrLn' "4: Square"
  putStr' "$ "
  osc <- liftIO getLine
  case osc of
    "1" -> do
      put (bpm, beats, oscillators ++ [Sinusoid []], notes, samples)
      getOscillators (x - 1)
    "2" -> do
      put (bpm, beats, oscillators ++ [Sawtooth []], notes, samples)
      getOscillators (x - 1)
    "3" -> do
      put (bpm, beats, oscillators ++ [Triangle []], notes, samples)
      getOscillators (x - 1)
    "4" -> do
      put (bpm, beats, oscillators ++ [Square []], notes, samples)
      getOscillators (x - 1)
    _ -> do
      liftIO $ putStrLn "Please select a number 1-4"
      getOscillators x

getNotes :: Int -> StateT SynthState IO ()
getNotes 0 = generateOscillatorSamples
getNotes x = do
  putStrLn' "Pick a note"
  putStr' "$ "
  note <- liftIO getLine
  case Map.lookup note notes of
    Just n -> do
      modify (\(bpm, beats, oscillators, notes, samples) -> (bpm, beats, oscillators, notes ++ [n], samples))
      getNotes (x - 1)
    Nothing -> do
      putStrLn' "Invalid note, try again."
      getNotes x

generateOscillatorSamples :: StateT SynthState IO ()
generateOscillatorSamples = do
  (bpm, beats, oscillators, notes, samples) <- get
  if null notes
    then do initSynthLoop
    else do
      let oscWithSample = map (oscillatorSample bpm beats (head notes)) oscillators
          interferenceOsc = map sample oscWithSample
      put (bpm, beats, oscillators, drop 1 notes, init samples ++ [interference (interferenceOsc ++ [last samples])])
  generateOscillatorSamples

oscillatorSample :: BPM -> Beats -> Frequency -> Oscillator -> Oscillator
oscillatorSample bpm beats freq (Sinusoid _) = Sinusoid (sinusoidWaveSample freq ((60 / fromIntegral bpm) * fromIntegral beats) sampleRate)
oscillatorSample bpm beats freq (Sawtooth _) = Sawtooth (sawtoothWaveSample freq ((60 / fromIntegral bpm) * fromIntegral beats) sampleRate)
oscillatorSample bpm beats freq (Triangle _) = Triangle (triangleWaveSample freq ((60 / fromIntegral bpm) * fromIntegral beats) sampleRate)
oscillatorSample bpm beats freq (Square _) = Square (squareWaveSample freq ((60 / fromIntegral bpm) * fromIntegral beats) sampleRate)

playSynth :: IO ()
playSynth = do
  (_, _, _, _, samples) <- execStateT synthesize (0, 0, [], [], [])
  putStrLn "Save file as:"
  putStr "$ "
  fileName <- getLine
  sound <- generateSound (concat samples) (maxBound `div` 10)
  writeWavFile sound fileName
  playSound fileName
