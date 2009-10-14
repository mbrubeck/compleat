import System.Posix.Env (getEnv)
import Completer (run, git)

main :: IO ()
main = do
    Just line <- getEnv "COMP_LINE"
    mapM_ putStrLn (run git line)
