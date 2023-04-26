module Parser where

-- import Text.Megaparsec.Char.Lexer hiding (float, space)

import Control.Monad.Combinators.Expr
import Data.Functor
import Data.Maybe (fromMaybe)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
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

opTable :: [[Operator Parser WaveExpr]]
opTable =
  [ [Postfix (string "x" $> Mult (Var "x"))],
    [ Prefix (string "sin" $> Sin),
      Prefix (string "cos" $> Cos)
    ],
    [InfixL (string "^" $> Exp)],
    [ InfixL (string "*" $> Mult),
      InfixL (string "/" $> Div)
    ],
    [ InfixL (string "+" $> Add),
      InfixL (string "-" $> Sub)
    ]
  ]

waveExpression :: Parser WaveExpr
waveExpression = makeExprParser term opTable

term :: Parser WaveExpr
term = Lit <$> float <|> Var <$> string "x" <|> betweenParen waveExpression

test = do
  let x = runParser waveExpression "" "2^2*2^2"
  case x of
    Left err -> putStrLn (errorBundlePretty err)
    Right x' -> do
      print x'
