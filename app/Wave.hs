{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Wave where

import Data.Fixed (mod')
import Data.List (transpose)

type SignalLength = Float

type SampleRate = Float

type Sample = (Float, Float)

type WaveVars =
  ( SignalLength,
    SampleRate,
    [[Sample]],
    FilePath
  )

data Wave = Wave
  { amplitude :: Float,
    frequency :: Float,
    phase :: Float,
    translation :: Float
  }
  deriving (Read, Show)

data WaveExpr
  = Lit Float
  | Var String
  | Add WaveExpr WaveExpr
  | Sub WaveExpr WaveExpr
  | Mult WaveExpr WaveExpr
  | Div WaveExpr WaveExpr
  | Exp WaveExpr WaveExpr
  | Sin WaveExpr
  | Cos WaveExpr
  | Asin WaveExpr
  | Acos WaveExpr
  | Mod WaveExpr WaveExpr
  | Floor WaveExpr
  | Signum WaveExpr
  deriving (Show, Read, Eq)

eval :: WaveExpr -> Float -> Float
eval (Lit x) _ = x
eval (Var n) v
  | n == "pi" = pi
  | otherwise = v
eval (Add x y) v = eval x v + eval y v
eval (Sub x y) v = eval x v - eval y v
eval (Mult x y) v = eval x v * eval y v
eval (Exp x y) v = eval x v ** eval y v
eval (Div x y) v = eval x v / eval y v
eval (Sin x) v = sin (eval x v)
eval (Cos x) v = cos (eval x v)
eval (Asin x) v = asin (eval x v)
eval (Acos x) v = acos (eval x v)
eval (Mod x y) v = eval x v `mod'` eval y v
eval (Floor x) v = fromIntegral $ floor (eval x v) :: Float
eval (Signum x) v = signum (eval x v)

createWaveSample :: Wave -> Float -> Float -> [Sample]
createWaveSample w len sampleRate = zip xCoords yCoords
  where
    delta = len / sampleRate
    xCoords = [0.0, 1 / sampleRate .. delta]
    -- xCoords = [0.0, delta .. len]
    yCoords = map (\x -> amplitude w * sin (2 * pi * frequency w * x + phase w) + translation w) xCoords

sinusoidWaveSample :: Float -> Float -> Float -> Float -> [Sample]
sinusoidWaveSample start freq len sampleRate = zip xCoords yCoords
  where
    xCoords = [start, start + 1 / sampleRate .. start + len]
    yCoords = map (\x -> sin (2 * pi * x * freq)) xCoords

sawtoothWaveSample :: Float -> Float -> Float -> Float -> [Sample]
sawtoothWaveSample start freq len sampleRate = zip xCoords yCoords
  where
    xCoords = [start, start + 1 / sampleRate .. start + len]
    yCoords = map (\x -> 2 * (freq * x - fromIntegral (floor (1 / 2 + freq * x)))) xCoords

triangleWaveSample :: Float -> Float -> Float -> Float -> [Sample]
triangleWaveSample start freq len sampleRate = zip xCoords yCoords
  where
    xCoords = [start, start + 1 / sampleRate .. start + len]
    yCoords = map (\x -> 2 * abs (2 * (freq * x - fromIntegral (floor (1 / 2 + freq * x)))) - 1) xCoords

squareWaveSample :: Float -> Float -> Float -> Float -> [Sample]
squareWaveSample start freq len sampleRate = zip xCoords yCoords
  where
    xCoords = [start, start + 1 / sampleRate .. start + len]
    yCoords = map (\x -> signum (sin (2 * pi * freq * x))) xCoords

interference :: [[Sample]] -> [Sample]
interference s = zip (map fst (head s)) yCoords
  where
    yCoords = map sum (transpose $ map (map snd) s)
