module LogAnalyzer (analyzeLog, constructTree, sortTree, filterErrors, LogEntry, LogTree) where

import Data.List.Split (splitOn)

-- Data Types
type TimeStamp = Int
data EntryType = Info | Warning | Error Int deriving (Show, Eq)
data LogEntry = LogEntry EntryType TimeStamp String | Unknown String deriving (Show, Eq)
data LogTree = Leaf | Node LogTree LogEntry LogTree deriving (Show, Eq)

-- Log Parsing
getLogEntryTimeStamp :: LogEntry -> TimeStamp
getLogEntryTimeStamp (Unknown _) = 0
getLogEntryTimeStamp (LogEntry _ timeStamp _) = timeStamp

analyzeLog :: String -> LogEntry
analyzeLog s
  | null s = Unknown ""
  | length entryType > 1 = Unknown s
  | head entryType == 'I' = LogEntry Info (read timeStamp) message
  | head entryType == 'W' = LogEntry Warning (read timeStamp) message
  | head entryType == 'E' = LogEntry (Error (read errorSeverity)) (read errorTimeStamp) errorMessage
  | otherwise = Unknown s
  where
    (entryType:timeStamp:messageParts) = splitOn " " s
    message = unwords messageParts
    (errorSeverity:errorTimeStamp:errorMessageParts) = messageParts
    errorMessage = unwords errorMessageParts

-- Tree Construction
insertEntry :: LogEntry -> LogTree -> LogTree
insertEntry (Unknown _) tree = tree
insertEntry logEntry Leaf = Node Leaf logEntry Leaf
insertEntry logEntry (Node leftTree nodeLogEntry rightTree)
  | logEntryTimeStamp <= nodeLogEntryTimeStamp = Node (insertEntry logEntry leftTree) nodeLogEntry rightTree
  | otherwise = Node leftTree nodeLogEntry (insertEntry logEntry rightTree)
  where
    logEntryTimeStamp = getLogEntryTimeStamp logEntry
    nodeLogEntryTimeStamp = getLogEntryTimeStamp nodeLogEntry

constructTree :: [LogEntry] -> LogTree
constructTree = foldr insertEntry Leaf

-- Tree Sorting
sortTree :: LogTree -> [LogEntry]
sortTree Leaf = []
sortTree (Node leftTree logEntry rightTree) = sortTree leftTree ++ [logEntry] ++ sortTree rightTree

-- Error Filtering
filterErrors :: [LogEntry] -> [String]
filterErrors = map getMessage . filter isSevereError
  where
    getMessage (LogEntry _ _ message) = message
    getMessage (Unknown message) = message
    isSevereError (LogEntry (Error severity) _ _) = severity >= 50
    isSevereError _ = False
    