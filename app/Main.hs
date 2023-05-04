{-# OPTIONS_GHC -Wno-type-defaults #-}

module Main where

import DFTVisualizer
import Synthesizer

main :: IO ()
main = do
  putStrLn "Please select what you want to do."
  putStrLn "1: Perform FFT on, and equalize a wave and hear the sound it produces"
  putStrLn "2: Use the synthesizer to create chord progressions and melodies"
  putStr "$ "
  choice <- read <$> getLine
  case choice of
    1 -> visualizeDFT
    2 -> playSynth
    _ -> do
      putStrLn "Please select 1 or 2"
      main
