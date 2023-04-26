module FFT where

import Data.Array.CArray
import Data.Complex
import Data.List
import Numeric.Transform.Fourier.FFT
import Wave

extractFreqs :: [Sample] -> [Float]
extractFreqs = sort . map fst . filter ((>= 0.001) . snd)

calcFFTmanual :: [Float] -> Int -> Int -> [Sample]
calcFFTmanual yCoords sampleRate signalLength = zip freqs magnitudes
  where
    n = sampleRate * signalLength
    yCoords' = listArray (0, length yCoords - 1) yCoords
    fftArr = rfft yCoords'
    xCoords = take (n `div` 2) [0 .. n]
    freqs = [0, 1 / (fromIntegral signalLength :: Float) .. fromIntegral n]
    magnitudes = map (\x -> magnitude x / (fromIntegral n :: Float)) (take (n `div` 2) (elems fftArr))

calcFFTwav :: [Float] -> Int -> Int -> [Sample]
calcFFTwav yCoords sampleRate signalLength = zip xCoords magnitudes
  where
    yCoords' = listArray (0, length yCoords - 1) yCoords
    fftArr = rfft yCoords'
    xCoords = map (fromIntegral :: Int -> Float) (take (signalLength `div` 2) [0 .. signalLength])
    magnitudes = map (\x -> (magnitude x :: Float) / fromIntegral sampleRate) (take (signalLength `div` 2) (elems fftArr))

calcFFT :: [Float] -> Int -> Int -> Int -> [Sample]
calcFFT yCoords sampleRate signalLength choice
  | choice == 1 = calcFFTmanual yCoords sampleRate signalLength
  | choice == 2 = calcFFTwav yCoords sampleRate signalLength
  | otherwise = error $ "Choice not supported" ++ show choice

decompose :: [Sample] -> Float -> Float -> IO [[Sample]]
decompose fftArr l s =
  do
    return samples
  where
    freqAmp = drop 1 $ sortBy (\(x1, _) (x2, _) -> compare x1 x2) (map (\(x, y) -> (x, y * 2)) (filter ((>= 0.001) . snd) fftArr))
    waves = map (\(freq, amp) -> Wave amp freq 0 0) (take 10 freqAmp)
    samples = map (\w -> createWaveSample w 10 100) waves
