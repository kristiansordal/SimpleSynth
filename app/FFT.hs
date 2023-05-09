{-# OPTIONS_GHC -Wno-type-defaults #-}

module FFT where

import Data.Array.CArray
import Data.Complex hiding (phase)
import Data.List
import Numeric.Transform.Fourier.FFT
import Wave

-- Computes the DFT using the FFT algorithm.
calcFFT :: [Float] -> Int -> Int -> [Sample]
calcFFT yCoords sampleRate signalLength = zip xCoords magnitudes
  where
    yCoords' = listArray (0, length yCoords - 1) yCoords
    fftArr = rfft yCoords'

    -- Half step the FFT to normalize the frequencies.
    xCoords = [0, 1 .. (fromIntegral sampleRate)]

    -- Data.Complex provides the magnitude function. Magnitude = Amplitude / 2
    magnitudes =
      getElems $
        map
          (\x -> (magnitude x :: Float) / fromIntegral sampleRate)
          (take (signalLength `div` 2) (elems fftArr))

-- Remove reduntant samples and get the ones we care about
getElems :: [Float] -> [Float]
getElems l = y
  where
    (evens, _) = partition (even . fst) (zip [0 ..] l)
    (_, y) = unzip evens

-- Decomposes the FFT array into its constituents. Frequencies with amplitudes <= 0.001 are filtered away to improve performance
decompose :: [Sample] -> [Wave]
decompose fftArr = waves
  where
    freqAmp = sortBy (\(_, amp1) (_, amp2) -> compare amp2 amp1) (map (\(x, y) -> (x, y)) (filter ((>= 0.001) . snd) fftArr))
    waves = map (\(freq, amp) -> Wave amp freq 0 0) freqAmp

equalize :: [Wave] -> IO [Wave]
equalize waves = do
  putStrLn "This is a simple equalizer used to remove or decrease the amplitude of certain frequencies in the wave."
  putStrLn $ "This wave is composed of " ++ show (length waves) ++ " waves with different frequencies and amplitudes."
  putStrLn "The 10 most significant of which are shown below: "
  printWaves (take 10 waves) 1
  putStrLn "Available equalizer functionality:"
  putStrLn "1: Equalize individual frequency"
  putStrLn "2: Low pass filter"
  putStrLn "3: High pass filter"
  putStr "$ "
  choice <- getLine
  case choice of
    "1" -> do
      putStrLn "Enter the index of the wave to equalize"
      putStr "$ "
      i <- getLine
      putStrLn "Enter the amount of reduction (in %) for the wave"
      putStr "$ "
      individual waves (read i) . read <$> getLine
    "2" -> do
      putStrLn "The low pass filter allows waves with a frequency lower than the cutoff pass through, and prevents waves with a higher frequency from passing through."
      putStrLn "Enter cutoff for the lowpass filter"
      putStr "$ "
      lowpass waves . read <$> getLine
    "3" -> do
      putStrLn "The high pass filter allows waves with a frequency higher than the cutoff pass through, and prevents waves with a lower frequency from passing through."
      putStrLn "Enter cutoff for the highpass filter"
      putStr "$ "
      highpass waves . read <$> getLine
    _ -> do
      putStrLn "Wrong input, please select either 1, 2 or 3"
      equalize waves

-- Filters frequencies higher than the cutoff
lowpass :: [Wave] -> Float -> [Wave]
lowpass w cutoff = filter (\(Wave _ freq _ _) -> freq <= cutoff) w

-- Filter frequencies lower than the cutoff
highpass :: [Wave] -> Float -> [Wave]
highpass w cutoff = filter (\(Wave _ freq _ _) -> freq >= cutoff) w

-- Reduces the amplitude, and thereby the volume of individual frequencies
individual :: [Wave] -> Int -> Float -> [Wave]
individual ws i reduction = ws'
  where
    wave = ws !! (i - 1)
    equalizedWave =
      wave
        { amplitude = amplitude wave * (reduction / 100),
          frequency = frequency wave,
          phase = phase wave,
          translation = translation wave
        }
    ws' = take (i - 1) ws ++ [equalizedWave] ++ drop i ws

-- Prints the frequency and amplitude of the waves
printWaves :: [Wave] -> Int -> IO ()
printWaves [] _ = return ()
printWaves (Wave amp freq _ _ : ws) i = do
  putStrLn $ show i ++ ": Amplitude: " ++ show amp ++ ", Frequency: " ++ show freq
  printWaves ws (i + 1)
