module Usage (fromFile) where

import qualified Completer as C
import Text.Parsec
import Text.Parsec.Language (javaStyle)
import Text.Parsec.String (Parser, parseFromFile)
import qualified Text.Parsec.Token as T

data Usage = Primitive C.Completer | Var String
             | Choice [Usage] | Sequence [Usage]
             | Many Usage | Many1 Usage | Optional Usage

fromFile :: String -> IO C.Completer
fromFile fileName = do
    result <- parseFromFile usage fileName
    case result of
        Right u  -> return (eval u)
        Left err -> error (show err)

-- Evaluator

eval :: Usage -> C.Completer
eval (Primitive c) = c
eval (Var s)       = if s == "file" then C.file else C.skip
eval (Choice xs)   = foldl1 (C.<|>) (map eval xs)
eval (Sequence xs) = foldl1 (C.-->) (map eval xs)
eval (Many x)      = C.many     (eval x)
eval (Many1 x)     = C.many1    (eval x)
eval (Optional x)  = C.optional (eval x)

-- Parser

usage :: Parser Usage
usage = do
    whiteSpace
    xs <- sepEndBy1 command (symbol ";")
    return (Choice xs)

command = do
    x <- commandName 
    y <- pattern
    return (Sequence [x, y])

commandName = atom >> return (Primitive C.skip)

pattern = do
    xs <- sepBy1 terms (symbol "|")
    return (Choice xs)

terms = do
    xs <- many1 term
    return (Sequence xs)

term = repeated (group <|> str <|> variable) Many1 id
   <|> repeated optionGroup Many Optional

group = parens pattern
optionGroup = brackets pattern

str = do
    s <- atom
    return $ Primitive (C.str s)

repeated :: Parser a -> (a -> b) -> (a -> b) -> Parser b
repeated p f g = p >>= \x ->
    try (symbol "..." >> return (f x)) <|> return (g x)

variable = do
    s <- between (symbol "<") (symbol ">") atom
    return (Var s)

atom :: Parser String
atom = stringLiteral <|> lexeme (many1 (alphaNum <|> oneOf "-_/@=+.,:"))

-- Lexer

lexer :: T.TokenParser ()
lexer  = T.makeTokenParser javaStyle

lexeme        = T.lexeme lexer
symbol        = T.symbol lexer
parens        = T.parens lexer
brackets      = T.brackets lexer
stringLiteral = T.stringLiteral lexer
whiteSpace    = T.whiteSpace lexer
