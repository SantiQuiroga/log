import LogAnalyzer (analyzeLog, constructTree, sortTree, filterErrors, LogEntry, LogTree)

testWhatWentWrong ::
  (String -> LogEntry) ->
  ([LogEntry] -> LogTree) ->
  (LogTree -> [LogEntry]) ->
  ([LogEntry] -> [String]) ->
  FilePath ->
  IO [String]
testWhatWentWrong parse builder getMessage checkWhatWentWrong file =
  checkWhatWentWrong . getMessage . builder . map parse . lines <$> readFile file

main :: IO ()
main = do
  testWhatWentWrong analyzeLog constructTree sortTree filterErrors "docs/sample.log" >>= print
  testWhatWentWrong analyzeLog constructTree sortTree filterErrors "docs/log_file.log" >>= print