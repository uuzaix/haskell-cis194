{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where
import Log

-- ex.1
parseMessage :: String -> LogMessage
-- parseMessage "E 2 562 help help" == LogMessage (Error 2) 562 "help help"
-- parseMessage "I 29 la la la" == LogMessage Info 29 "la la la"
-- parseMessage "This is not in the right format" == Unknown "This is not in the right format"

parseMessage message = case (words message) of
    "I":t:m -> LogMessage Info (read t) (unwords m)
    "W":t:m -> LogMessage Warning (read t) (unwords m)
    "E":s:t:m -> LogMessage (Error (read s) ) (read t) (unwords m)
    _ -> Unknown message

parse :: String -> [LogMessage]
parse file = map parseMessage (lines file)




