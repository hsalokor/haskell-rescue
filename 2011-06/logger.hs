import Control.Monad(when)

data Logger = Logger { info :: Log, warn :: Log, error :: Log }
data Level = Info|Warn|Error deriving (Show, Ord, Eq)
type Log = String -> IO ()

sysOutLogger threshold = Logger (log Info) (log Warn) (log Error)
  where log lvl msg = when (lvl >= threshold) $ putStrLn $ show lvl ++ " - " ++ msg


