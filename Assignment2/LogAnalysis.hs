{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

parseMessage :: String -> LogMessage
parseMessage str = let wordList = words str in
  case wordList of
    ("E":en:ts:msg) -> LogMessage (Error (read en)) (read ts) (unwords msg)
    ("I":ts:msg) -> LogMessage Info (read ts) (unwords msg)
    ("W":ts:msg) -> LogMessage Warning (read ts) (unwords msg)
    _ -> Unknown (unwords wordList)

parse :: String -> [LogMessage]
parse = map parseMessage . lines

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert message Leaf = Node Leaf message Leaf
insert newMessage@(LogMessage _ newTimeStamp _) (Node left oldMessage@(LogMessage _ oldTimeStamp _) right) =
    if newTimeStamp > oldTimeStamp
      then Node left oldMessage (insert newMessage right)
      else Node (insert newMessage left) oldMessage right
insert _ tree = tree

build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left message right) = inOrder left ++ [message] ++ inOrder right

isSevere :: LogMessage -> Bool
isSevere (LogMessage (Error severeNum) _ _) = severeNum > 50
isSevere _ = False

toString :: LogMessage -> String
toString (LogMessage _ _ message) = message
toString (Unknown message) = message

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = map toString . filter isSevere . inOrder . build
