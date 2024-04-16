-- file: Logger.hs
-- author: Jacob Xie
-- date: 2024/03/09 09:48:52 Saturday
-- brief:

module Logger
  ( Logger,
    Log,
    runLogger,
    record,
  )
where

type Log = [String]

runLogger :: Logger a -> (a, Log)
runLogger = undefined

globToRegex :: String -> Logger String
globToRegex = undefined

record :: String -> Logger ()
record = undefined
