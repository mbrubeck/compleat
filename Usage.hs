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
        Right env -> return (eval env $ mainCommand env)
        Left err  -> error (show err)

-- Evaluator

type Environment = [(String,Usage)]

eval :: Environment -> Usage -> C.Completer
eval env (Primitive c) = c
eval env (Choice xs)   = foldl1 (C.<|>) (map (eval env) xs)
eval env (Sequence xs) = foldl1 (C.-->) (map (eval env) xs)
eval env (Many x)      = C.many     (eval env x)
eval env (Many1 x)     = C.many1    (eval env x)
eval env (Optional x)  = C.optional (eval env x)
eval env (Var s)       = if s == "file" then C.file else C.skip

mainCommand :: Environment -> Usage
mainCommand env = Choice $ map snd $ filter ((""==) . fst) env

-- Top-level parser

usage :: Parser [(String, Usage)]
usage = whiteSpace >> sepEndBy1 (try definition <|> mainDefinition) (symbol ";")

definition :: Parser (String, Usage)
definition = do
    s <- atom
    symbol "="
    u <- pattern
    return (s, u)

mainDefinition :: Parser (String, Usage)
mainDefinition = do
    u <- command
    return ("", u)

-- Usage parser

command :: Parser Usage
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

variable = do
    s <- between (symbol "<") (symbol ">") atom
    return (Var s)

repeated :: Parser a -> (a -> b) -> (a -> b) -> Parser b
repeated p f g = p >>= \x ->
    try (symbol "..." >> return (f x)) <|> return (g x)

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
