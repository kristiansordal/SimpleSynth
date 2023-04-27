module Utils where

import Control.Monad.State
import Text.Read

-- Function for a generic input of a float / number with text
inputFloat :: String -> IO Float
inputFloat s = do
  putStr s
  input <- getLine
  case (readMaybe input :: Maybe Float) of
    Just num -> return num
    Nothing -> do
      putStrLn "Incorrect type, try again."
      inputFloat s

putStrLn' s = liftIO $ putStrLn s

putStr' s = liftIO $ putStr s
