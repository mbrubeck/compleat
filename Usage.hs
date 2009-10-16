module Usage () where

import Text.Parsec.Char (alphaNum, char, oneOf)
import Text.Parsec.Combinator (anyToken, between, chainl1, many1)
import Text.Parsec.Prim ((<|>), many, try, runParser)
import Text.Parsec.String (Parser)
import qualified Completer as C
import qualified Text.Parsec.Token as T
import Text.Parsec.Language (javaStyle)

-- Example

git :: C.Completer
git = run choice "git [-a|-b] ...  ( add [-i|--amend] ... | commit [-b|-m <msg>] ... )"

run :: Parser a -> String -> a
run p s = case runParser p () "" s of
            Right a -> a
            Left err -> error (show err)

-- Grammar

choice = chainl1 terms (symbol "|" >> return (C.<|>))

terms :: Parser C.Completer
terms = do
    cs <- many term
    return (foldl (C.-->) C.continue cs)

term = repeated (group <|> str) C.many1 id
   <|> repeated optionGroup C.many C.optional
   <|> variable

group = parens choice
optionGroup = brackets choice

str = do
    s <- stringLiteral <|> lexeme (many1 (alphaNum <|> oneOf "-_/@=+.,"))
    return (C.str s)

repeated :: Parser a -> (a -> b) -> (a -> b) -> Parser b
repeated p f g = do
    c <- p
    try (symbol "..." >> return (f c)) <|> return (g c)

variable = do
    between (char '<') (char '>') identifier
    return C.skip

-- Lexer

lexer :: T.TokenParser ()
lexer  = T.makeTokenParser javaStyle

lexeme        = T.lexeme lexer
symbol        = T.symbol lexer
identifier    = T.identifier lexer
parens        = T.parens lexer
brackets      = T.brackets lexer
stringLiteral = T.stringLiteral lexer
