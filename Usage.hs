module Usage () where

import Text.Parsec.Char (alphaNum, oneOf)
import Text.Parsec.Combinator (anyToken, chainl1, many1)
import Text.Parsec.Prim ((<|>), many, try, runParser)
import Text.Parsec.String (Parser)
import qualified Completer as C
import qualified Text.Parsec.Token as T
import Text.Parsec.Language (javaStyle)

-- Example

git :: C.Completer
git = run terms "git [-a|-b] ...  ( add [-i|-m|--amend] | commit -b )"

run :: Parser a -> String -> a
run p s = case runParser p () "" s of
            Right a -> a
            Left err -> error (show err)

-- Grammar

terms :: Parser C.Completer
terms = do
    cs <- many term
    return (foldl (C.-->) C.continue cs)

term = repeated (group <|> str) C.many1 id
   <|> repeated optionGroup C.many C.optional

group = parens (try choice <|> terms)
optionGroup = brackets (try choice <|> terms)

choice = do
    c <- terms
    symbol "|"
    d <- (try choice <|> terms)
    return (c C.<|> d)

str = do
    s <- stringLiteral <|> lexeme (many1 (alphaNum <|> oneOf "-_/@=+.,"))
    return (C.str s)

repeated :: Parser a -> (a -> b) -> (a -> b) -> Parser b
repeated p f g = do
    c <- p
    try (symbol "..." >> return (f c)) <|> return (g c)

-- Lexer

lexer :: T.TokenParser ()
lexer  = T.makeTokenParser javaStyle

whiteSpace    = T.whiteSpace lexer
lexeme        = T.lexeme lexer
symbol        = T.symbol lexer
parens        = T.parens lexer
brackets      = T.brackets lexer
stringLiteral = T.stringLiteral lexer
