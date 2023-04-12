module FFT where

import Data.List
import Wave

extractFreqs :: [Sample] -> [Float]
extractFreqs = sort . map fst . filter ((>= 0.001) . snd)
