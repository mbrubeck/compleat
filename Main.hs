import System.Posix.Env (getEnv)
import Completer (Completer, run)
import Usage (usage)

git :: Completer
git = usage $ "git [-a|-b] ...  ( add [-i|--amend] ... | commit [-b|-m <msg>] ... ) "
         ++ "| git foo bar baz"

main :: IO ()
main = do
    Just line <- getEnv "COMP_LINE"
    mapM_ putStrLn (run git line)
