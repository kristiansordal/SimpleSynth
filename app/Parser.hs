module Parser where

-- import Text.Megaparsec.Char.Lexer hiding (float, space)

import Control.Monad.Combinators.Expr
import Data.Functor
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

-- Parses a double
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

opTable :: [[Operator Parser Float]]
opTable =
  [ [ InfixL (symbol space "*" $> (*)),
      InfixL (symbol space "/" $> (/))
    ],
    [ InfixL (symbol space "+" $> (+)),
      InfixL (symbol space "-" $> (-))
    ],
    [ Prefix (string "sin" *> optional space $> sin),
      Prefix (string "cos" *> optional space $> cos)
    ]
  ]

term :: Parser Float
term = float <|> betweenParen expression

expression :: Parser Float
expression = makeExprParser term opTable

opTable' :: [[Operator Parser WaveExpr]]
opTable' =
  [ [ InfixL (optional space *> symbol space "*" *> optional space $> Mult),
      InfixL (optional space *> symbol space "/" *> optional space $> Div)
    ],
    [ InfixL (symbol space "+" *> optional space $> Add),
      InfixL (symbol space "-" *> optional space $> Sub)
    ],
    [ Prefix (string "sin" *> optional space $> Sin),
      Prefix (string "cos" *> optional space $> Cos)
    ]
  ]

waveExpression :: Parser WaveExpr
waveExpression = makeExprParser term' opTable'

term' :: Parser WaveExpr
term' = Lit <$> decimal <|> Var <$> string "x" <|> betweenParen waveExpression

test = do
  let x = runParser expression "" "2.0*(sin(1.0+10.0))+1.0"
  case x of
    Left err -> putStrLn (errorBundlePretty err)
    Right x' -> print x'
