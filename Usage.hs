module Usage (fromFile) where

import qualified Completer as C
import Text.Parsec
import Text.Parsec.Language (javaStyle)
import Text.Parsec.String (Parser, parseFromFile)
import qualified Text.Parsec.Token as T

fromFile :: String -> IO C.Completer
fromFile fileName = do
    result <- parseFromFile usage fileName
    case result of
        Right c  -> return c
        Left err -> error (show err)

usage :: Parser C.Completer
usage = do
    whiteSpace
    cs <- sepEndBy1 command (symbol ";")
    return $ foldl1 (C.<|>) cs

command = do
    c <- commandName 
    s <- pattern
    return (c C.--> s)

commandName = atom >> return C.skip

pattern = chainl1 terms (symbol "|" >> return (C.<|>))

terms = do
    cs <- many term
    return (foldl (C.-->) C.continue cs)

term = repeated (group <|> str <|> variable) C.many1 id
   <|> repeated optionGroup C.many C.optional

group = parens pattern
optionGroup = brackets pattern

str = atom >>= \s -> return (C.str s)

repeated :: Parser a -> (a -> b) -> (a -> b) -> Parser b
repeated p f g = do
    c <- p
    try (symbol "..." >> return (f c)) <|> return (g c)

variable = do
    id <- between (symbol "<") (symbol ">") atom
    return (if id == "file" then C.file else C.skip)

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
