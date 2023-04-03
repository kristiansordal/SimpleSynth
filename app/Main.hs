{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Main where

import Data.Array.CArray
import Data.Complex
import Data.List
import Data.List.Extra
import Graphics.Matplotlib
import Numeric.Transform.Fourier.FFT
import Parser
import System.Random
import Text.Megaparsec
import Text.Read
import Wave

genericInput :: String -> IO Float
genericInput s = do
  putStr s
  input <- getLine
  case (readMaybe input :: Maybe Float) of
    Just num -> return num
    Nothing -> do
      putStrLn "Incorrect type, try again."
      genericInput s

inputWave :: Float -> Float -> IO [Sample]
inputWave l s = do
  putStrLn "Select input mode: "
  putStrLn "1: Manual input"
  putStr "2: Expression input "
  selection <- getLine
  case selection of
    "1" -> do
      freq <- genericInput "Enter wave frequency: "
      amp <- genericInput "Enter wave amplitude: "
      phase <- genericInput "Enter wave phase: "
      trans <- genericInput "Enter waves translation: "
      let w = Wave amp freq phase trans
      return $ createWaveSample w l s
    "2" -> inputWaveExpression l s
    _ -> do
      putStrLn "Invalid choice, try again."
      inputWave l s

inputWaveExpression :: Float -> Float -> IO [Sample]
inputWaveExpression l s = do
  putStrLn "Enter expression for wave: "
  input <- getLine
  let input' = filter (/= ' ') input
  let xCoords = [0.0, 1 / s .. l]
  let parsed = runParser waveExpression "" input'
  case parsed of
    Left err -> do
      putStrLn (errorBundlePretty err)
      inputWaveExpression l s
    Right x -> do
      let yCoords = map (eval x) xCoords
      print x
      return (zip xCoords yCoords)

nWaves :: Int -> Float -> Float -> IO [[Sample]]
nWaves 0 _ _ = return []
nWaves n l s =
  do
    w <- inputWave l s
    ws <- nWaves (n - 1) l s
    return (w : ws)

genRandomWaves :: Int -> Float -> Float -> [[Sample]]
genRandomWaves 0 _ _ = []
genRandomWaves n l s =
  createWaveSample (Wave (head randNums) (randNums !! 1) (randNums !! 2) (randNums !! 3)) l s : genRandomWaves (n - 1) l s
  where
    gen = mkStdGen n
    randNums = take 10 $ randomRs (1.0, s) gen

getWaves :: String -> Int -> Float -> Float -> IO [[Sample]]
getWaves str n l s
  | str == "y" = return (genRandomWaves n l s)
  | str == "n" = nWaves n l s
  | otherwise = error "Wrong input"

calcDFT tp sampling =
  let arr = listArray (0, length tp - 2) tp
      dftArr = rfft arr
      magnitudes = map (\(x :+ _) -> sqrt (x * x)) (take (length dftArr `div` 2) (elems dftArr))
      magnitudes' = map (\(x :+ _) -> 2 / 100 * abs x) (take (length dftArr `div` 2) (elems dftArr))
      freqBins = map (\x -> (x * sampling) `div` length dftArr) [0 .. length dftArr]
      freqBins' = [0.0, (1 / fromIntegral sampling) .. fromIntegral $ maximum freqBins]
      groups = map length (group freqBins)
   in zip freqBins magnitudes

calcDFT2 yCoords sampleRate signalLength = zip freqs magnitudes
  where
    n = sampleRate * signalLength
    yCoords' = listArray (0, length yCoords - 1) yCoords
    fftArr = rfft yCoords'
    xCoords = take (n `div` 2) [0 .. n]
    freqs = [0, 1 / (fromIntegral signalLength :: Float) .. fromIntegral n]
    magnitudes = map (\x -> (2 * magnitude x) / (fromIntegral n :: Float)) (take (n `div` 2) (elems fftArr))

-- magnitudes' = map (/ n) magnitudes

fixPlot [] = []
fixPlot (x : xs)
  | snd x > 0 = (fst x, 0) : x : (fst x, 0) : fixPlot xs
  | otherwise = x : fixPlot xs

main :: IO ()
main =
  do
    n <- genericInput "How many waves? "
    l <- genericInput "Enter length of waves: "
    s <- genericInput "Enter sampling rate: "
    putStr "Generate random waves? (y / n) "
    rand <- getLine
    ws <- getWaves rand (round n) l s
    let ws' = interference ws
        ws'' = ws' : ws
        dftArr = calcDFT2 (map snd ws') (round s) (round l)
        xCoords = map fst ws'
        plots = [setSubplot (round n - i) % plot xCoords (map snd w) | (i, w) <- zip [0 ..] ws'']
        func = foldr (%) mp (reverse plots)

    onscreen $
      subplots
        @@ [o2 "nrows" (round n + 1), o2 "ncols" 1, o2 "linewidth" 10]
        % setSizeInches 10 8
        % func
        % subplots
        @@ [o2 "nrows" 2, o2 "ncols" 1, o2 "linewidth" 25]
        % setSizeInches 10 8
        % setSubplot 0
        % title "Time Domain"
        % plot xCoords (map snd ws')
        % setSubplot 1
        % title "Frequency Domain"
        % plot (map fst dftArr) (map snd dftArr)
