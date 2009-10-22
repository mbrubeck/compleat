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
usage = whiteSpace >> set

set = chainl1 terms (symbol "|" >> return (C.<|>))

terms = do
    cs <- many term
    return (foldl (C.-->) C.continue cs)

term = repeated (group <|> str <|> variable) C.many1 id
   <|> repeated optionGroup C.many C.optional

group = parens set
optionGroup = brackets set

str = do
    s <- stringLiteral <|> lexeme (many1 (alphaNum <|> oneOf "-_/@=+.,"))
    return (C.str s)

repeated :: Parser a -> (a -> b) -> (a -> b) -> Parser b
repeated p f g = do
    c <- p
    try (symbol "..." >> return (f c)) <|> return (g c)

variable = lexeme $ do
    id <- between (char '<') (char '>') identifier
    return (if id == "file" then C.file else C.skip)

-- Lexer

lexer :: T.TokenParser ()
lexer  = T.makeTokenParser javaStyle

lexeme        = T.lexeme lexer
symbol        = T.symbol lexer
identifier    = T.identifier lexer
parens        = T.parens lexer
brackets      = T.brackets lexer
stringLiteral = T.stringLiteral lexer
whiteSpace    = T.whiteSpace lexer
