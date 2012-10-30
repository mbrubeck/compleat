import Completer (run)
import Numeric (readDec)
import System.Environment (getEnv, getArgs)
import Tokenize (tokenize)
import Usage (Environment, commands, fromFile, lookupCommand)

-- Parse the usage file from the first argument.  If there is a second argument,
-- it is a command-name; find the usage rules for that command and use them to
-- complete the input line.  Otherwise, list all command names in the usage file.
main = do
    args <- getArgs
    env <- fromFile (head args)
    if length args > 1
        then completeLine env (args !! 1)
        else listCommands env

completeLine :: Environment -> String -> IO ()
completeLine env command = do
    line <- getInput
    let completer = lookupCommand env command
    suggestions <- run completer (tokenize line)
    mapM_ putStrLn suggestions

getInput :: IO String
getInput = do
    line  <- getEnv "COMP_LINE"
    point <- getEnv "COMP_POINT"
    let [(n,[])] = readDec point
    return (take n line)

listCommands :: Environment -> IO ()
listCommands = mapM_ putStrLn . commands
