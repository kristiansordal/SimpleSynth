{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module Main where

import Control.Monad
-- import Data.Aeson
import Graphics.Matplotlib
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

inputWave :: IO Wave
inputWave = do
  freq <- genericInput "Enter wave frequency: "
  amp <- genericInput "Enter wave amplitude: "
  phase <- genericInput "Enter wave phase: "
  trans <- genericInput "Enter waves translation: "
  len <- genericInput "Enter wave length: "
  sampling <- genericInput "Enter sampling rate: "

  return $ createWave amp freq phase len sampling trans

nWaves :: Int -> IO [Wave]
nWaves 0 = return []
nWaves n =
  do
    w <- inputWave
    ws <- nWaves (n - 1)
    return (w : ws)

plots =
  onscreen $
    subplots
      @@ [o2 "nrows" 1, o2 "ncols" 1]
      % setSubplot 0
      % plot [0 .. 1] [0 .. 1]

main :: IO ()
main =
  do
    -- putStr "How many waves? "
    -- n <- getLine
    n <- genericInput "How many waves? "
    ws <- nWaves (round n)
    let ws' = interference ws
        ws'' = ws' : map samples ws
        xCoords = map fst ws'
    -- plots = [setSubplot i % plot (toJSON xCoords) (toJSON (samples w)) | (i, w) <- zip [0 ..] ws]

    onscreen $
      subplots
        @@ [o2 "nrows" 1, o2 "ncols" 1]
        % setSizeInches 24 13
        % setSubplot 0
        % plot (map fst ws') (map snd ws')
        % setSubplot 1
        % plot (map fst ws') (map snd ws')
