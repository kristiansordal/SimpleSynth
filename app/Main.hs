{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module Main where

import Data.Array.CArray
import Data.Complex
import Graphics.Matplotlib
-- import Math.FFT
-- import Numeric.Transform.Fourier.DFT
import Numeric.Transform.Fourier.FFT
import System.Random
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

inputWave :: Float -> Float -> IO Wave
inputWave l s = do
  freq <- genericInput "Enter wave frequency: "
  amp <- genericInput "Enter wave amplitude: "
  phase <- genericInput "Enter wave phase: "
  trans <- genericInput "Enter waves translation: "

  return $ createWave amp freq phase trans l s

nWaves :: Int -> Float -> Float -> IO [Wave]
nWaves 0 _ _ = return []
nWaves n l s =
  do
    w <- inputWave l s
    ws <- nWaves (n - 1) l s
    return (w : ws)

genRandomWaves :: Int -> Float -> Float -> [Wave]
genRandomWaves 0 _ _ = []
genRandomWaves n l s =
  createWave (head randNums) (randNums !! 1) (randNums !! 2) (randNums !! 3) l s : genRandomWaves (n - 1) l s
  where
    gen = mkStdGen n
    randNums = take 10 $ randomRs (0.0, 10.0) gen

getWaves :: String -> Int -> Float -> Float -> IO [Wave]
getWaves str n l s
  | str == "y" = return (genRandomWaves n l s)
  | str == "n" = nWaves n l s
  | otherwise = error "Wrong input"

-- calcDFT :: [Float] -> Float -> [Float]
calcDFT tp sampling =
  let complexArr = listArray (0, length tp - 2) tp
      dftArr = rfft complexArr
      freqBins = map (\x -> (x * sampling) `div` length dftArr) [0 .. length dftArr]
   in take (length freqBins `div` 2) freqBins

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
        ws'' = ws' : map samples ws
        dftArr = calcDFT (map snd ws') (round s)
    print dftArr

    --         xCoords = map fst ws'
    --         plots = [setSubplot (round n - i) % plot xCoords (map snd w) | (i, w) <- zip [0 ..] ws'']
    --         func = foldr (%) mp (reverse plots)
    --         yCoords = map snd ws'
    --         c = array (0, length yCoords - 1) (zip [0 ..] (map (:+ 0) yCoords))
    --     -- dftArr = fftr (listArray (0, length `div` 2) (elems c))
    --     -- dftList = map (\(x :+ _) -> abs x / s) (elems dftArr)
    --     -- pl = zip [0 ..] (drop 1 (take (length dftList `div` 2) dftList))
    onscreen $
      subplots
        @@ [o2 "nrows" (round n + 1), o2 "ncols" 1]
        % setSizeInches 10 8
        % setSubplot 0
        % plot [0 ..] dftArr

--         % func
--         % subplots
--         @@ [o2 "nrows" 2, o2 "ncols" 1]
--         % setSizeInches 10 8
--         % setSubplot 0
--         % title "Time Domain"
--         % plot (map fst ws') (map snd ws')
--         % setSubplot 1
--         % title "Frequency Domain"

-- -- % plot (map fst pl) (map snd pl)
