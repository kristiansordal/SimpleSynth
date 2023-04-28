{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Main where

import Control.Monad.State
import FFT
import Graphics.Matplotlib
import Parser
import Sound
import System.CPUTime
import System.IO
import System.Process
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
  freq <- liftIO $ inputFloat "Enter wave frequency: "
  amp <- liftIO $ inputFloat "Enter wave amplitude: "
  phase <- liftIO $ inputFloat "Enter wave phase: "
  trans <- liftIO $ inputFloat "Enter waves translation: "
  let w = Wave amp freq phase trans
  addSample (createWaveSample w l s)

-- Create wave based on a sum of sine and cosine functions
inputWaveExpression :: StateT WaveState IO ()
inputWaveExpression = do
  (l, s, _, _, _) <- get
  liftIO $ putStrLn "Enter expression for wave: "
  input <- liftIO getLine
  let input' = filter (/= ' ') input
      xCoords = [0.0, 1 / s .. l]
      parsed = runParser waveExpression "" input'
  case parsed of
    Left err -> do
      liftIO $ putStrLn (errorBundlePretty err)
      inputWaveExpression
    Right x -> do
      let yCoords = map (eval x) xCoords
      addSample (zip xCoords yCoords)

genRandomWave :: StateT WaveState IO ()
genRandomWave = do
  (s, l, _, _, _) <- get
  seed <- liftIO getCPUTime
  let gen = mkStdGen (fromIntegral seed)
      randNums = take 10 $ randomRs (1.0, s) gen
      w = createWaveSample (Wave (head randNums) (randNums !! 1) (randNums !! 2) (randNums !! 3)) l s
  addSample w

getWaves :: Int -> StateT WaveState IO ()
getWaves 0 = return ()
getWaves n =
  do
    liftIO $ putStrLn "Select input mode: "
    liftIO $ putStrLn "1: Manual input"
    liftIO $ putStrLn "2: Expression input"
    liftIO $ putStrLn "3: Generate random wave"
    liftIO $ putStr "$ "
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
  (_, Just hout, _, _) <- liftIO $ createProcess (proc "python3" ["wavreader.py", fileName]) {std_out = CreatePipe}
  h <- lines <$> liftIO (hGetContents hout)
  let sampleRate = read (head h) :: Int
      sineWave = zip [0 ..] (read (last h) :: [Float])
  put (fromIntegral $ length sineWave, fromIntegral sampleRate, [sineWave], fileName, mode)

modeSelection :: StateT WaveState IO ()
modeSelection = do
  liftIO $ putStrLn "Select Mode: "
  liftIO $ putStrLn "1: Manual wave entry"
  liftIO $ putStrLn "2: Read .wav file"
  liftIO $ putStr "$ "
  choice <- liftIO getLine
  case choice of
    "1" -> do
      selectionManual
      (l, s, samp, _, _) <- get
      addSample (interference samp)
      liftIO $ putStrLn "Store file as (default: wave.wav): "
      liftIO $ putStr "$ "
      fileName <- liftIO getLine
      if null fileName
        then put (l, s, samp, "wave.wav", "1")
        else put (l, s, samp, fileName, "1")
    "2" -> do
      liftIO $ putStrLn "Enter filename and filepath (if not in current directory) of a .wav file"
      liftIO $ putStr "$ "
      fileName <- liftIO getLine
      put (0, 0, [], fileName, "2")
      readWavFile
    _ -> do
      liftIO $ putStrLn "Wrong input, please select either 1 or 2"
      modeSelection

selectionManual :: StateT WaveState IO ()
selectionManual = do
  n <- liftIO $ inputFloat "How many waves? "
  l <- liftIO $ inputFloat "Enter length of waves: "
  s <- liftIO $ inputFloat "Enter sampling rate: "
  put (l, s, [], "", "")
  getWaves (round n)

loop :: WaveInformation -> IO ()
loop (length, sampleRate, samples, fileName, c) = do
  let fftArr = calcFFT (map snd (head samples)) sampleRate length c
      waves = decompose fftArr
  plotFigure samples fftArr

  -- play the sound from the wave generated
  wave <- generateSound (head samples) (maxBound `div` 10)
  writeWavFile wave fileName
  playSound ("wavfiles/" ++ fileName)

  eq <- equalize waves
  let samples' = map (\x -> createWaveSample x (fromIntegral length :: Float) (fromIntegral sampleRate :: Float)) eq
      samples'' = interference samples' : samples'
  loop (length, sampleRate, samples, fileName, c)

plotFigure :: [[Sample]] -> [Sample] -> IO ()
plotFigure samples fftArr = do
  let waves = decompose fftArr
      xCoords = map fst (head samples)
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
  (l, s, samples, fileName, c) <- execStateT modeSelection (0, 0, [], "", "")
  loop (round l, round s, samples, fileName, read c)
