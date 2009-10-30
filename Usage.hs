module Usage (Environment, commands, fromFile, lookupCommand) where

import qualified Completer as C
import Data.List (nub, sort)
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language (javaStyle)
import qualified Text.ParserCombinators.Parsec.Token as T

-- This module parses the usage file format (see README for an explanation)
-- and generates a Completer (see the Completer module).

data Usage = Primitive C.Completer | Var String
             | Choice [Usage] | Sequence [Usage]
             | Many Usage | Many1 Usage | Optional Usage
             | ShellCommand String

fromFile :: String -> IO Environment
fromFile fileName = do
    result <- parseFromFile usage fileName
    case result of
        Right env -> return env
        Left err  -> error (show err)

-- Evaluator

type Environment = [(EnvName,Usage)] -- Associates variables with values.
data EnvName = VarName String | CommandName String
    deriving Eq

lookupCommand :: Environment -> String -> C.Completer
lookupCommand env command = eval env (main env)
    where main env = Choice $ map snd $ filter ((CommandName command ==) . fst) env

eval :: Environment -> Usage -> C.Completer
eval env (Primitive c) = c
eval env (Choice xs)   = foldl1 (C.<|>) (map (eval env) xs)
eval env (Sequence xs) = foldl1 (C.-->) (map (eval env) xs)
eval env (Many x)      = C.many     (eval env x)
eval env (Many1 x)     = C.many1    (eval env x)
eval env (Optional x)  = C.optional (eval env x)
eval env (ShellCommand s) = C.shellCommand s
eval env (Var s)       = case lookup (VarName s) env of
                            Just u  -> eval env u
                            Nothing -> C.skip

commands :: Environment -> [String]
commands env = nub $ sort [s | (CommandName s, _) <- env]

-- Top-level parser

usage :: Parser Environment
usage = whiteSpace >> sepEndBy1 (try varDef <|> commandDef) (symbol ";")

varDef :: Parser (EnvName, Usage)
varDef = do
    s <- atom
    symbol "="
    u <- shellCommand <|> pattern
    return (VarName s, u)

commandDef :: Parser (EnvName, Usage)
commandDef = do
    s <- atom
    u <- pattern
    return (CommandName s, Sequence [Primitive C.skip, u])

-- Usage parser

shellCommand = do
    symbol "!"
    s <- many1 (noneOf ";")
    return (ShellCommand s)

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
