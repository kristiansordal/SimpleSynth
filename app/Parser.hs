module Parser where

-- import Text.Megaparsec.Char.Lexer hiding (float, space)

import Control.Monad.Combinators.Expr
import Data.Functor
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer hiding (float, space)
import Wave

type Parser = Parsec Void String

-- Takes a parser and returns a parser of whats between the parenthesis
betweenParen :: Parser a -> Parser a
betweenParen = between (char '(') (char ')')

-- This parser is necessary to allow for both integers without decimal points
-- to be parsed into float, aswell as numbers with decimal points
float :: Parser Float
float = do
  ds <- some digitChar
  _ <- optional $ char '.'
  fs <- optional $ some digitChar
  let ds' = if null ds then "0" else ds
      fs' = fromMaybe "0" fs
      ds'' = read ds'
      fs'' = read $ "0." ++ fs'
  return $ ds'' + fs''

-- Operator table to make it easy to parse mathematical expressions
opTable :: [[Operator Parser WaveExpr]]
opTable =
  [ [ Postfix
        ( do
            x <- some alphaNumChar
            return $ Mult (Var x)
        )
    ],
    [ Prefix (string "sin" $> Sin),
      Prefix (string "cos" $> Cos),
      Prefix (string "asin" $> Asin),
      Prefix (string "acos" $> Acos),
      Prefix (string "floor" $> Floor),
      Prefix (string "sgn" $> Signum)
    ],
    [InfixL (string "^" $> Exp)],
    [ InfixL (string "*" $> Mult),
      InfixL (string "/" $> Div),
      InfixL (string "%" $> Mod)
    ],
    [ InfixL (string "+" $> Add),
      InfixL (string "-" $> Sub)
    ]
  ]

waveExpression :: Parser WaveExpr
waveExpression = makeExprParser term opTable

parseVar :: Parser WaveExpr
parseVar = do
  v <- some alphaNumChar
  return (Var v)

term :: Parser WaveExpr
term = Lit <$> float <|> parseVar <|> betweenParen waveExpression

-- Triangle Wave
-- (2a/pi)*asin(sin((2pi/p)*x))

-- Sawtooth Wave
-- 2*(x/p-floor(1/2 + 2*x/p))

-- Square Wave
-- sgn(sin(x))
--
-- Cool Sawtooth + Triangle combinations
-- (2a/pi)*asin(sin((2pi/0.025)*x)) + 2*(x/0.0025-floor(1/2 + 2*x/0.0025))
-- (2/pi)*asin(sin((2pi/0.005)*x)) + 2*(x/0.025-floor(1/2 + 2*x/0.025))

test = do
  -- let x = runParser waveExpression "" "sgn(sin(x)) + 2*(x/0.025-floor(1/2 + 2*x/0.025))"
  let x = runParser waveExpression "" "(2a/pi)*asin(sin((2pi/0.025)*x)) + 2*(x/0.025-floor(1/2 + 2*x/0.025))"

  case x of
    Left err -> putStrLn (errorBundlePretty err)
    Right x' -> do
      print x'
