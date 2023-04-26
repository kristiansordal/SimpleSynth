{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Wave where

import Data.Fixed
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe

type Sample = (Float, Float)

variableState :: Map String Float
variableState = Map.empty

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

data Wave = Wave
  { amplitude :: Float,
    frequency :: Float,
    phase :: Float,
    translation :: Float
  }
  deriving (Read, Show)

createWaveSample :: Wave -> Float -> Float -> [Sample]
createWaveSample w len sampling = zip xCoords yCoords
  where
    xCoords = [0.0, 1 / sampling .. len]
    yCoords = map (\x -> amplitude w * sin (2 * pi * frequency w * x + phase w) + translation w) xCoords

interference :: [[Sample]] -> [Sample]
interference s = zip (map fst (head s)) yCoords
  where
    yCoords = map sum (transpose $ map (map snd) s)

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
eval (Sin x) v = sin (2 * pi * eval x v)
eval (Cos x) v = cos (2 * pi * eval x v)
eval (Asin x) v = asin (eval x v)
eval (Acos x) v = acos (eval x v)
eval (Mod x y) v = eval x v `mod'` eval y v
eval (Floor x) v = fromIntegral $ floor (eval x v) :: Float
eval (Signum x) v = signum (eval x v)
