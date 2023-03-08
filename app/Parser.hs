module Parser where

import Data.Char
import Data.List.Extra
import Data.Maybe
import Text.Regex
import Wave

-- parseWave :: String -> Wave
-- parseWave s = read . parse
--   where
--     parse s
--       | length $ takeWhile (isDigit) s >= 1 = "Wave {amplitude = " ++ (takeWhile isDigit s) : parse (dropWhile isDigit s)
