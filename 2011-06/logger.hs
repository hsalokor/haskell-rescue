import Control.Monad(when)

data Logger = Logger { info :: Log, warn :: Log, error :: Log }
data Level = Info|Warn|Error deriving (Show, Ord, Eq)
type Log = String -> IO ()

sysOutLogger threshold = Logger (log Info) (log Warn) (log Error)
  where log level msg = when (level >= threshold) $ putStrLn $ (show level) ++ " - " ++ msg


