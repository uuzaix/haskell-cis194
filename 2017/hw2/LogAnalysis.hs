module LogAnalysis where

import Log

parseMessage :: String -> LogMessage
parseMessage ms = case  words ms of
  "I":t:x -> LogMessage Info (read t) (unwords x)
  "W":t:x -> LogMessage Warning (read t) (unwords x) 
  "E":i:t:x -> LogMessage (Error (read i)) (read t) (unwords x)
  _ -> Unknown ms

parse :: String -> [LogMessage]
parse f = map parseMessage (lines f)

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _)  tree= tree 
insert m Leaf = Node Leaf m Leaf
insert new@(LogMessage _ time1 _) (Node tree1 old@(LogMessage _ time2 _) tree2) = 
  if time1 < time2 then 
    Node (insert new tree1) old tree2
    else Node tree1 old (insert new tree2)

build :: [LogMessage] -> MessageTree
build [] = Leaf
build (m:ms) = insert m (build ms)

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node t1 m t2) = (inOrder t1) ++ [m] ++ (inOrder t2)

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong ms = map (\(LogMessage (Error _) _ m ) -> m ) (inOrder (build (filter f ms))) where 
  f :: LogMessage -> Bool
  f (LogMessage (Error t) _ _ ) = t >= 50
  f _ = False