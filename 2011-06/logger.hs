data Logger = Logger { info :: Log, warn :: Log, error :: Log }
type Log = String -> IO ()
sysOutLogger = Logger (log "Info") (log "Warn") (log "Error")
  where log level msg = putStrLn $ level ++ " - " ++ msg
