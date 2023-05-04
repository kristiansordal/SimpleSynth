{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module DFTVisualizer where

import Control.Monad.State
import Data.WAVE
import FFT
import Graphics.Matplotlib
import Parser
import Sound
import System.CPUTime
import System.Random
import Text.Megaparsec hiding (State)
import Utils
import Wave

type WaveInformation = (Int, Int, [Sample], String, Int)

type WaveState = StateT WaveVars' IO ()

-- Adds a sample to the wave state
addSample :: [Sample] -> WaveState
addSample newSample = modify (\(l, s, samples, fileName) -> (l, s, samples ++ [newSample], fileName))

-- Get the interference of the samples once all wave samples have been obtained
processSamples :: WaveState
processSamples = modify (\(l, s, samples, fileName) -> (l, s, interference samples : samples, fileName))

-- Create wave samples by manually entering the various fields of the wave
inputWaveManual :: WaveState
inputWaveManual = do
  (l, s, _, _) <- get
  freq <- inputFloat' "Enter wave frequency: "
  amp <- inputFloat' "Enter wave amplitude: "
  phs <- inputFloat' "Enter wave phase: "
  trns <- inputFloat' "Enter waves translation: "
  let w = Wave amp freq phs trns
  addSample (createWaveSample w l s)

-- Create wave based on a sum of sine and cosine functions
inputWaveExpression :: WaveState
inputWaveExpression = do
  (l, s, _, _) <- get
  putStrLn' "Enter expression for wave: "
  input <- liftIO getLine
  let input' = filter (/= ' ') input
      xCoords = [0.0 .. l]
      parsed = runParser waveExpression "" input'
  case parsed of
    Left err -> do
      -- Pretty print the error messsage provided by the parser before
      -- retrying
      putStrLn' (errorBundlePretty err)
      inputWaveExpression
    Right x -> do
      -- Normalize the x and y coords by dividing by the samplerate
      let yCoords = map (\i -> eval x (i / s)) xCoords
      addSample (zip (map (/ s) xCoords) yCoords)

-- Generates a random wave with frequencies, translation, and amplitude
-- in the range 1 <= x <= signalLength. This results in white noise.
genRandomWave :: WaveState
genRandomWave = do
  (l, s, _, _) <- get
  -- Use the cpu time as a seed to guarantee unique values for every wave
  seed <- liftIO getCPUTime
  let gen = mkStdGen (fromIntegral seed)
      randNums = take 4 $ randomRs (1.0, l) gen
      w =
        createWaveSample
          ( Wave
              (head randNums)
              (randNums !! 1)
              (randNums !! 2)
              (randNums !! 3)
          )
          l
          s
  addSample w

-- Reads a wave file at the given file locations
readWavFile :: WaveState
readWavFile = do
  putStrLn' "Please enter the filename of a valid .wav file"
  putStr' "$ "
  fileName <- liftIO getLine
  wave <- liftIO $ getWAVEFile ("wavfiles/" ++ fileName)
  let (samples, sampleRate) = processWaveFile wave
  put
    ( fromIntegral $ length samples,
      fromIntegral sampleRate,
      [samples],
      fileName
    )

-- Processes a wave file into a tuple of samples and sampleRate
processWaveFile :: WAVE -> ([Sample], Int)
processWaveFile (WAVE header samples) = (zip xCoords yCoords, waveFrameRate header)
  where
    yCoords = map (realToFrac . sampleToDouble) $ concat samples
    xCoords = [0.0, 1 / fromIntegral (waveFrameRate header) ..]

-- Gets the amount of waves user has selected. Each wave can be generated as the user pleases
-- When n is zero, the interference of the waves obtained is calculated with the processSamples function
getWaves :: Int -> WaveState
getWaves 0 = processSamples
getWaves n =
  do
    putStrLn' "Select input mode: "
    putStrLn' "1: Manual input"
    putStrLn' "2: Expression input"
    putStrLn' "3: Generate random wave"
    putStr' "$ "
    c <- (read :: String -> Int) <$> liftIO getLine
    case c of
      1 -> inputWaveManual
      2 -> inputWaveExpression
      3 -> genRandomWave
      _ -> do
        putStrLn' "Wrong input. Please select either 1, 2 or 3"
        getWaves n
    getWaves (n - 1)

-- Sets the global information about the wave such as number of waves, their length and samplerate
waveInformation :: WaveState
waveInformation = do
  n <- (round :: Float -> Int) <$> inputFloat' "How many waves? "
  l <- inputFloat' "Enter length of waves: "
  s <- inputFloat' "Enter sampling rate: "
  put (l * s, s, [], "")
  getWaves n
  storeFile

-- Stores the file at a given location
storeFile :: WaveState
storeFile = do
  putStrLn' "Store file as: "
  putStr' "$ "
  fileName <- liftIO getLine
  if null fileName
    then do
      putStrLn' "Please enter a file name."
      storeFile
    else do
      (l, s, samples, _) <- get

      -- maxBound sets the volume of the wave file, dividing by 10 proved to be a decent volume.
      sound <- liftIO $ generateSound (head samples) (maxBound `div` 10) (round s)

      liftIO $ writeWavFile sound fileName
      put (l, s, samples, fileName)

-- Plots the wave and its DFT using bindings to Matplotlib
plotFigure :: [Sample] -> [Sample] -> IO ()
plotFigure samples fftArr = do
  let xCoords = map fst samples
      yCoords = map snd samples
      xCoordsFFT = map fst fftArr
      yCoordsFFT = map snd fftArr

  -- Onscreen is the function used to open a matplotlib plot
  -- It is composed with the two subplots, one for the wave,
  -- and another for the DFT
  onscreen $
    subplots
      @@ [o2 "nrows" 2, o2 "ncols" 1]
      % setSizeInches 10 8
      % setSubplot 0
      % title "Time Domain"
      % plot xCoords yCoords
      @@ [o2 "linewidth" 0.5]
      % setSubplot 1
      % title "Frequency Domain"
      % plot xCoordsFFT yCoordsFFT
      @@ [o2 "linewidth" 1]

loop :: WaveState
loop = do
  (len, sampleRate, samples, fileName) <- get
  let interferenceSamples = head samples

      -- Compute the DFT of the interference of the samples
      fftArr = calcFFT (map snd interferenceSamples) (round sampleRate) (round len)
      waves = decompose fftArr

  -- Plot the wave and play the sound it makes
  liftIO $ plotFigure interferenceSamples fftArr
  liftIO $ playSound ("wavfiles/" ++ fileName)

  -- Get a list of type [Wave] by equalizing the waves
  eq <- liftIO $ equalize waves

  -- Generate new wavesamples based on the waves
  let samples' = map (\x -> createWaveSample x len sampleRate) eq

  put (len, sampleRate, samples', fileName)
  processSamples
  storeFile
  loop

modeSelection :: WaveState
modeSelection = do
  putStrLn' "Select Mode: "
  putStrLn' "1: Manual wave entry"
  putStrLn' "2: Read .wav file"
  putStr' "$ "
  mode <- (read :: String -> Int) <$> liftIO getLine
  case mode of
    1 -> waveInformation
    2 -> readWavFile
    _ -> do
      putStrLn' "Wrong choice, please select 1 or 2"
      modeSelection
  loop

visualizeDFT :: IO ()
visualizeDFT = do
  (_, _, _, _) <- execStateT modeSelection (0, 0, [], "")
  putStrLn' "Goodbye!"
