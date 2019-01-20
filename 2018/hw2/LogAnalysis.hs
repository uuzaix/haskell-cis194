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


-- ex.2
insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown m) tree = tree
insert m Leaf = Node Leaf m Leaf
insert @new(LogMessage _ time1 _) (Node tree1 @old(LogMessage _ time2 _) tree2)
    | time1 > time2  = Node tree1 old (insert new tree2)
    | otherwise = Node (insert new tree1) old tree2


-- ex.3
build :: [LogMessage] -> MessageTree
build [] = Leaf
build (m:ms) = insert m (build ms)


-- ex.4
inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node tree1 m tree2) = (inOrder tree1) ++ [m] ++ (inOrder tree2)
