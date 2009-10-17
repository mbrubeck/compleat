import Completer (run)
import Numeric (readDec)
import System (getArgs)
import System.Environment (getEnv)
import Usage (fromFile)

main = do
    line <- getInput
    args <- getArgs
    completer <- fromFile (head args)
    mapM_ putStrLn (run completer line)

getInput :: IO String
getInput = do
    line  <- getEnv "COMP_LINE"
    point <- getEnv "COMP_POINT"
    let [(n,[])] = readDec point
    return (take n line)
