{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Main where

import Control.Monad.State
import Data.WAVE
import FFT
import Graphics.Matplotlib
import Parser
import Sound
import Synthesizer
import System.CPUTime
import System.Random
import Text.Megaparsec hiding (State)
import Utils
import Wave

type WaveInformation = (Int, Int, [[Sample]], String, Int)

addSample :: [Sample] -> StateT WaveState IO ()
addSample newSample = do
  (l, s, samples, fileName, mode) <- get
  put (l, s, samples ++ [newSample], fileName, mode)

-- Create wave samples by manually entering the various fields of the wave
inputWaveManual :: StateT WaveState IO ()
inputWaveManual = do
  (l, s, _, _, _) <- get
  freq <- inputFloat' "Enter wave frequency: "
  amp <- inputFloat' "Enter wave amplitude: "
  phase <- inputFloat' "Enter wave phase: "
  trans <- inputFloat' "Enter waves translation: "
  let w = Wave amp freq phase trans
  addSample (createWaveSample w l s)

-- Create wave based on a sum of sine and cosine functions
inputWaveExpression :: StateT WaveState IO ()
inputWaveExpression = do
  (l, s, _, _, _) <- get
  putStrLn' "Enter expression for wave: "
  input <- liftIO getLine
  let input' = filter (/= ' ') input
      xCoords = [0.0 .. l]
      parsed = runParser waveExpression "" input'
  case parsed of
    Left err -> do
      putStrLn' (errorBundlePretty err)
      inputWaveExpression
    Right x -> do
      let yCoords = map (\i -> eval x (i / s)) xCoords
      addSample (zip (map (/ s) xCoords) yCoords)

genRandomWave :: StateT WaveState IO ()
genRandomWave = do
  (l, s, _, _, _) <- get
  seed <- liftIO getCPUTime
  let gen = mkStdGen (fromIntegral seed)
      randNums = take 4 $ randomRs (1.0, l) gen
      w = createWaveSample (Wave (head randNums) (randNums !! 1) (randNums !! 2) (randNums !! 3)) l s
  addSample w

getWaves :: Int -> StateT WaveState IO ()
getWaves 0 = return ()
getWaves n =
  do
    putStrLn' "Select input mode: "
    putStrLn' "1: Manual input"
    putStrLn' "2: Expression input"
    putStrLn' "3: Generate random wave"
    putStr' "$ "
    c <- liftIO getLine
    case c of
      "1" -> inputWaveManual
      "2" -> inputWaveExpression
      "3" -> genRandomWave
      _ -> error "Wrong input."
    getWaves (n - 1)

readWavFile :: StateT WaveState IO ()
readWavFile = do
  (_, _, _, fileName, mode) <- get
  wave <- liftIO $ getWAVEFile ("wavfiles/" ++ fileName)
  let (samples, sampleRate) = processWaveFile wave

  put (fromIntegral $ length samples, fromIntegral sampleRate, [samples], fileName, mode)

processWaveFile :: WAVE -> ([Sample], Int)
processWaveFile (WAVE header samples) = (zip xCoords yCoords, waveFrameRate header)
  where
    yCoords = map (realToFrac . sampleToDouble) $ concat samples
    xCoords = [0.0, 1 / fromIntegral (waveFrameRate header) ..]

modeSelection :: StateT WaveState IO ()
modeSelection = do
  putStrLn' "Select Mode: "
  putStrLn' "1: Manual wave entry"
  putStrLn' "2: Read .wav file"
  putStr' "$ "
  choice <- liftIO getLine
  case choice of
    "1" -> do
      selectionManual
      (l, s, samp, _, _) <- get
      addSample (interference samp)
      putStrLn' "Store file as (default: wave.wav): "
      putStr' "$ "
      fileName <- liftIO getLine
      if null fileName
        then do
          put (l, s, samp, "wave.wav", "1")
          wave <- liftIO $ generateSound (head samp) (maxBound `div` 10)
          liftIO $ writeWavFile wave "wave.wav"
        else do
          put (l, s, samp, fileName, "1")
          wave <- liftIO $ generateSound (head samp) (maxBound `div` 10)
          liftIO $ writeWavFile wave fileName
    "2" -> do
      putStrLn' "Enter filename and filepath (if not in current directory) of a .wav file"
      putStr' "$ "
      fileName <- liftIO getLine
      put (0, 0, [], fileName, "2")
      readWavFile
    _ -> do
      putStrLn' "Wrong input, please select either 1 or 2"
      modeSelection

selectionManual :: StateT WaveState IO ()
selectionManual = do
  n <- inputFloat' "How many waves? "
  l <- inputFloat' "Enter length of waves: "
  s <- inputFloat' "Enter sampling rate: "
  put (l * s, s, [], "", "")
  getWaves (round n)

loop :: WaveInformation -> IO ()
loop (len, sampleRate, samples, fileName, c) = do
  let fftArr = calcFFT (map snd (head samples)) sampleRate len
      waves = decompose fftArr
  plotFigure samples fftArr
  playSound ("wavfiles/" ++ fileName)

  eq <- equalize waves
  let samples' = map (\x -> createWaveSample x (fromIntegral len :: Float) (fromIntegral sampleRate :: Float)) eq
      samples'' = interference samples' : samples'

  -- play the sound from the wave generated
  wave <- generateSound (head samples'') maxBound
  putStr "Save copy as: "
  fileName' <- getLine
  if null fileName'
    then do
      writeWavFile wave fileName
      loop (len, sampleRate, samples'', fileName, c)
    else do
      writeWavFile wave fileName'
      loop (len, sampleRate, samples'', fileName', c)

plotFigure :: [[Sample]] -> [Sample] -> IO ()
plotFigure samples fftArr = do
  let xCoords = map fst (head samples)
      yCoords = map snd (head samples)
      xCoordsFFT = map fst fftArr
      yCoordsFFT = map snd fftArr

  -- plot the wave generated aswell as the FFT of the wave
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

main :: IO ()
main = do
  putStrLn "Please select what you want to do."
  putStrLn "1: Perform FFT on, and equalize a wave and hear the sound it produces"
  putStrLn "2: Use the synthesizer to create chord progressions and melodies"
  putStr "$ "
  choice <- getLine
  case choice of
    "1" -> do
      (l, s, samples, fileName, c) <- execStateT modeSelection (0, 0, [], "", "")
      loop (round l, round s, samples, fileName, read c)
    "2" -> playSynth
    _ -> do
      putStrLn "Please select 1 or 2"
      main
