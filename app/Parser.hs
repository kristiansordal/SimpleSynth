module Parser where

import Data.Maybe
import Data.Void
import Numeric
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Read (readMaybe)
import Wave

type Parser a = Parsec Void String a

-- Takes a parser and returns a parser of whats between the parenthesis
betweenParen :: Parser a -> Parser a
betweenParen = between (char '(' *> space) (space <* char ')')

-- Parses a float
parseDoubleOrFloat :: (Num a, Read a) => Parser a
parseDoubleOrFloat = do
  ds <- some digitChar
  _ <- optional $ char '.'
  fs <- optional $ some digitChar
  let ds' = if null ds then "0" else ds
      fs' = fromMaybe "0" fs
      ds'' = read ds'
      fs'' = read $ "0." ++ fs'
  return $ ds'' + fs''

-- parseNum :: (Num a, Read a) => Parser a
-- parseNum = do
--   num <- some digitChar
--   dot <- optional $ char '.'
--   case dot of
--     --   Nothing -> return (read num)
--     --   Just dot' -> do
--     --     decimal <- some digitChar
--     --     return (read (num ++ [dot'case dot of

parseSigned :: Parser Float
parseSigned = do
  sign <- char '+' <|> char '-'
  num <- optional space *> parseDoubleOrFloat
  case sign of
    '+' -> return num
    '-' -> return (negate num)
    _ -> fail "Illegal sign."

parseOrdFreq :: Parser Float
parseOrdFreq = do
  _ <- string "2pi"
  optional space *> parseDoubleOrFloat

parseAngFreq :: Parser Float
parseAngFreq = do
  angFreq <- parseDoubleOrFloat
  _ <- optional space *> string "rad/s"
  return (angFreq / (2 * pi))

parseFrequency :: Parser Float
parseFrequency = parseOrdFreq <|> parseAngFreq

parseWave :: Parser Wave
parseWave = do
  amp <- parseDoubleOrFloat
  _ <- optional space *> (string "sin" <|> string "cos")
  _ <- char '('
  f <- optional space *> parseFrequency
  p <- optional space *> parseSigned
  _ <- char ')'
  t <- space *> parseSigned
  return (Wave amp f p t)

testParser = do
  let x = runParser parseWave "" "2sin(1.75rad/s-1)+2"
  case x of
    Left err -> putStrLn (errorBundlePretty err)
    Right x' -> print x'
