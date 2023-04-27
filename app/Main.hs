{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Main where

import FFT
import Graphics.Matplotlib
import Parser
import Sound
import System.CPUTime
import System.IO
import System.Process
import System.Random
import Text.Megaparsec
import Text.Read
import Wave

type WaveInformation = (Int, Int, Int, [[Sample]], String)

-- Function for a generic input of a float / number with text
genericInput :: String -> IO Float
genericInput s = do
  putStr s
  input <- getLine
  case (readMaybe input :: Maybe Float) of
    Just num -> return num
    Nothing -> do
      putStrLn "Incorrect type, try again."
      genericInput s

-- Create wave samples by manually entering the various fields of the wave
inputWaveManual :: Float -> Float -> IO [Sample]
inputWaveManual l s = do
  freq <- genericInput "Enter wave frequency: "
  amp <- genericInput "Enter wave amplitude: "
  phase <- genericInput "Enter wave phase: "
  trans <- genericInput "Enter waves translation: "
  let w = Wave amp freq phase trans
  return $ createWaveSample w l s

-- Create wave based on a sum of sine and cosine functions
inputWaveExpression :: Float -> Float -> IO [Sample]
inputWaveExpression l s = do
  putStrLn "Enter expression for wave: "
  input <- getLine
  let input' = filter (/= ' ') input
      xCoords = [0.0, 1 / s .. l]
      parsed = runParser waveExpression "" input'
  case parsed of
    Left err -> do
      putStrLn (errorBundlePretty err)
      inputWaveExpression l s
    Right x -> do
      print x
      let yCoords = map (eval x) xCoords
      return (zip xCoords yCoords)

inputWave :: Float -> Float -> IO [Sample]
inputWave l s = do
  putStrLn "Select input mode: "
  putStrLn "1: Manual input"
  putStrLn "2: Expression input "
  selection <- getLine
  case selection of
    "1" -> inputWaveManual l s
    "2" -> inputWaveExpression l s
    _ -> do
      putStrLn "Invalid choice, try again."
      inputWave l s

-- TODO Needs random seed
genRandomWave :: Float -> Float -> IO [Sample]
genRandomWave l s = do
  seed <- getCPUTime
  let gen = mkStdGen (fromIntegral seed)
      randNums = take 10 $ randomRs (1.0, s) gen
  return $ createWaveSample (Wave (head randNums) (randNums !! 1) (randNums !! 2) (randNums !! 3)) l s

getWaves :: Int -> Float -> Float -> IO [[Sample]]
getWaves 0 _ _ = return []
getWaves n l s =
  do
    putStrLn "Select input mode: "
    putStrLn "1: Manual input"
    putStrLn "2: Expression input"
    putStrLn "3: Generate random wave"
    putStr "$ "
    c <- getLine
    w <- case c of
      "1" -> inputWaveManual l s
      "2" -> inputWaveExpression l s
      "3" -> genRandomWave l s
      _ -> error "Wrong input."
    ws <- getWaves (n - 1) l s
    return (w : ws)

-- TODO: Error handling
readWavFile :: String -> IO (Int, Int, [[Sample]])
readWavFile fileName = do
  (_, Just hout, _, _) <- createProcess (proc "python3" ["wavreader.py", fileName]) {std_out = CreatePipe}
  h <- lines <$> hGetContents hout
  let sampleRate = read (head h) :: Int
      sineWave = zip [0 ..] (read (last h) :: [Float])
  return (length sineWave, sampleRate, [sineWave])

modeSelection :: IO WaveInformation
modeSelection = do
  putStrLn "Select Mode: "
  putStrLn "1: Manual wave entry"
  putStrLn "2: Read .wav file"
  putStr "$ "
  choice <- getLine
  case choice of
    "1" -> do
      (l, s, samples) <- selectionManual
      let samples' = interference samples : samples
      putStrLn "Store file as (default: wave.wav): "
      putStr "$ "
      fileName <- getLine
      (if null fileName then return (1, l, s, samples', "wave.wav") else return (1, l, s, samples', fileName))
    "2" -> do
      putStrLn "Enter filename and filepath (if not in current directory) of a .wav file"
      putStr "$ "
      fileName <- getLine
      (l, s, samples) <- readWavFile fileName
      return (2, l, s, samples, "wavfiles/" ++ fileName)
    _ -> error "Wrong input"

selectionManual :: IO (Int, Int, [[Sample]])
selectionManual = do
  n <- genericInput "How many waves? "
  l <- genericInput "Enter length of waves: "
  s <- genericInput "Enter sampling rate: "
  samples <- getWaves (round n) l s
  return (round l, round s, samples)

main :: IO ()
main = do
  (choice, l, s, samples, fileName) <- modeSelection
  loop (choice, l, s, samples, fileName)

-- let fftArr = calcFFT (map snd (head samples)) s l choice
--     waves = decompose fftArr
-- plotFigure samples fftArr

-- -- play the sound from the wave generated
-- wave <- generateSound (head samples) (maxBound `div` 10)
-- writeWavFile wave fileName
-- playSound ("wavfiles/" ++ fileName)

-- loop :: WaveInformation -> IO ()
loop (choice, length, sampleRate, samples, fileName) = do
  let fftArr = calcFFT (map snd (head samples)) sampleRate length choice
      waves = decompose fftArr
  plotFigure samples fftArr

  -- play the sound from the wave generated
  wave <- generateSound (head samples) (maxBound `div` 10)
  writeWavFile wave fileName
  playSound ("wavfiles/" ++ fileName)

  eq <- equalize waves
  let samples' = map (\x -> createWaveSample x (fromIntegral length :: Float) (fromIntegral sampleRate :: Float)) eq
      samples'' = interference samples' : samples'
  loop (choice, length, sampleRate, samples'', fileName)

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
