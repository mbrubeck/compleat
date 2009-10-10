import Control.Monad (MonadPlus, mzero, mplus)
import Data.List (isPrefixOf)
import Parser (tokenize)

-- Example

git :: Completer [String]
git = str "git" --> gitOptions --> gitCommand

gitOptions :: Completer [String]
gitOptions = str "--version" <|> str "--help" <|> str "--work-tree"

gitCommand :: Completer [String]
gitCommand = (str "add" --> (str "-i" <|> str "-n" <|> str "-v"))
         <|> (str "commit" --> (str "-m" <|> str "-a" <|> str "--amend"))

-- Completers

data Completer a = Completer { complete :: ([String] -> [(a, [String])]) }

run :: Completer [a] -> String -> [a]
run c s = concat $ map fst $ complete c (tokenize s)

instance Monad Completer where
    return a  = Completer (\ts -> [(a, ts)])
    c >>= f   = Completer (\ts -> concat [complete (f a) ts' | (a, ts') <- complete c ts])

instance MonadPlus Completer where
    mzero     = Completer (\_  -> [])
    mplus c d = Completer (\ts -> complete c ts ++ complete d ts)

(<|>) :: Completer a -> Completer a -> Completer a
(<|>) = mplus

getInput :: Completer [String]
getInput = Completer (\ts -> [(ts, ts)])

(-->) :: Completer a -> Completer a -> Completer a
c --> d = do
    a <- c
    ts <- getInput
    case ts of [] -> return a
               _  -> d

str :: String -> Completer [String]
str s = Completer (\ts -> case ts of
                            []     -> []
                            [t]    -> if t `isPrefixOf` s then [([s],[])] else []
                            (t:ts) -> if t == s           then [([s],ts)] else [])
