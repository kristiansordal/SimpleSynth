{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module Main where

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

inputWave :: Float -> Float -> IO Wave
inputWave l s = do
  freq <- genericInput "Enter wave frequency: "
  amp <- genericInput "Enter wave amplitude: "
  phase <- genericInput "Enter wave phase: "
  trans <- genericInput "Enter waves translation: "

  return $ createWave amp freq phase l s trans

nWaves :: Int -> Float -> Float -> IO [Wave]
nWaves 0 _ _ = return []
nWaves n l s =
  do
    w <- inputWave l s
    ws <- nWaves (n - 1) l s
    return (w : ws)

main :: IO ()
main =
  do
    n <- genericInput "How many waves? "
    l <- genericInput "Enter length of waves: "
    s <- genericInput "Enter sampling rate: "
    ws <- nWaves (round n) l s
    let ws' = interference ws
        ws'' = ws' : map samples ws
        xCoords = map fst ws'
        plots = [setSubplot (i - 1) % plot xCoords (map snd w) | (i, w) <- zip [0 ..] ws'']
        func = foldl (%) mp plots
    print (length ws'')
    print (zip [0 ..] ws'')
    onscreen $
      subplots
        @@ [o2 "nrows" (round n + 1), o2 "ncols" 1]
        % setSizeInches 10 10
        % func
