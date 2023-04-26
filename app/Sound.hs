{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Sound where

import Control.Monad (when)
import Data.Default.Class (def)
import Data.Int (Int32)
import Data.WAVE
import qualified SDL
import qualified SDL.Mixer as Mix
import Wave

header = WAVEHeader 1 48000 32 Nothing

generateSound :: [Sample] -> Int32 -> Int -> Int -> IO WAVE
generateSound samples volume bitrate hz = do
  return $ WAVE header (map (: []) sampleVol)
  where
    sampleVol = map (round . (* fromIntegral volume) . snd) samples :: [Int32]

writeWavFile :: WAVE -> IO ()
writeWavFile = putWAVEFile "wave.wav"

whileTrueM :: Monad m => m Bool -> m ()
whileTrueM cond = do
  loop <- cond
  when loop $ whileTrueM cond

playSound :: String -> IO ()
playSound file = do
  -- open device
  Mix.openAudio def 256

  -- open file
  sound <- Mix.load file

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
