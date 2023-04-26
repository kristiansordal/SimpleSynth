{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Sound where

import Control.Monad (when)
import Data.Default.Class (def)
import Data.Int (Int32)
import Data.WAVE
import qualified SDL
import qualified SDL.Mixer as Mix
import Wave

generateSound :: [Sample] -> Int32 -> Int -> Int -> IO WAVE
generateSound samples volume bitrate hz = do
  return $ WAVE header (map (: []) sampleVol)
  where
    sampleVol = map (round . (* fromIntegral volume) . snd) samples :: [Int32]
    header = WAVEHeader 1 48000 bitrate Nothing

writeWavFile :: WAVE -> IO ()
writeWavFile = putWAVEFile "wave.wav"

-- waveData = WAVE header samples

playSound :: IO ()
playSound = do
  -- open device
  Mix.openAudio def 256

  -- open file
  sound <- Mix.load "wave.wav"

  -- play file
  Mix.play sound

  -- wait until finished
  whileTrueM $ Mix.playing Mix.AllChannels

  -- free resources
  Mix.free sound

  -- close device
  Mix.closeAudio

  -- quit
  Mix.quit
  SDL.quit

whileTrueM :: Monad m => m Bool -> m ()
whileTrueM cond = do
  loop <- cond
  when loop $ whileTrueM cond
