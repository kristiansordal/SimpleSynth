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

generateSound :: [Sample] -> Int32 -> IO WAVE
generateSound samples volume = do
  return $ WAVE header (map (: []) sampleVol)
  where
    sampleVol = map (round . (* fromIntegral volume) . snd) samples :: [Int32]

writeWavFile :: WAVE -> String -> IO ()
writeWavFile wave fileName = putWAVEFile ("wavfiles/" ++ fileName) wave

whileTrueM :: Monad m => m Bool -> m ()
whileTrueM cond = do
  loop <- cond
  when loop $ whileTrueM cond

playSound :: String -> IO ()
playSound file = do
  Mix.openAudio def 256

  putStrLn "Loading file..."
  sound <- Mix.load file

  -- Mix.play sound

  putStrLn "♫♫♫♫♫ sound playing ♫♫♫♫♫"
  -- whileTrueM $ Mix.playing Mix.AllChannels
  repeatSound sound

  Mix.free sound

  Mix.closeAudio

  Mix.quit
  SDL.quit

repeatSound sound = do
  Mix.play sound
  whileTrueM $ Mix.playing Mix.AllChannels
  repeatSound sound
