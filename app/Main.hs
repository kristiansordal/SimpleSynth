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
import System.IO
import System.Process
import System.Random
import Text.Megaparsec
import Text.Read
import Wave

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
genRandomWave l s =
  return $ createWaveSample (Wave (head randNums) (randNums !! 1) (randNums !! 2) (randNums !! 3)) l s
  where
    gen = mkStdGen 10
    randNums = take 10 $ randomRs (1.0, s) gen

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

modeSelection :: IO (Int, Int, Int, [[Sample]])
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
      return (1, l, s, samples')
    "2" -> do
      putStrLn "Enter filename and filepath (if not in current directory) of a .wav file"
      putStr "$ "
      fileName <- getLine
      (l, s, samples) <- readWavFile fileName
      return (2, l, s, samples)
    _ -> error "Wrong input"

selectionManual :: IO (Int, Int, [[Sample]])
selectionManual = do
  n <- genericInput "How many waves? "
  l <- genericInput "Enter length of waves: "
  s <- genericInput "Enter sampling rate: "
  samples <- getWaves (round n) l s
  return (round l, round s, samples)

main :: IO ()
main =
  do
    (choice, l, s, samples) <- modeSelection
    let freqs = extractFreqs dftArr
        dftArr = calcFFT (map snd (head samples)) s l choice
    samples' <- decompose dftArr (fromIntegral l :: Float) (fromIntegral s :: Float)
    let n = length samples'
        xCoords = map fst (head samples')
        plots = [setSubplot (n - i) % plot xCoords (map snd w) | (i, w) <- zip [0 ..] samples']
        func = foldr (%) mp (reverse plots)

    onscreen $
      subplots
        @@ [o2 "nrows" (n + 1), o2 "ncols" 1]
        % setSizeInches 10 8
        % func
        % subplots
        @@ [o2 "nrows" 2, o2 "ncols" 1]
        % setSizeInches 10 8
        % setSubplot 0
        % title "Time Domain"
        % plot (map fst (head samples)) (map snd (head samples))
        @@ [o2 "linewidth" 0.5]
        % setSubplot 1
        % title "Frequency Domain"
        % plot (map fst dftArr) (map snd dftArr)
        @@ [o2 "linewidth" 1]

    -- play the sound from the wave generated
    wave <- generateSound (head samples) (maxBound `div` 2) 32 (length samples `div` l)
    writeWavFile wave
    playSound
