module VMPrinter where

runPrompt :: IO ()
runPrompt = do putStr "SAMMY> "
               ln <- getLine
               checkAndExec ln >> runPrompt

menuTable :: [(String, String)]
menuTable = [ ("help", "Display help menu")
            , ("pacc", "Print accumulator register")
            , ("pmem", "Print memory location contents")
            , ("quit", "Quit the virtual machine")
            ]

help :: IO ()
help = do putStrLn "Possible commands are:\n"
          mapM_ (\x -> putStrLn (fst x ++ "\t\t" ++ snd x)) menuTable 

run :: String -> IO ()
run = undefined

checkAndExec :: String -> IO ()
checkAndExec ln = if cmd `elem` map fst menuTable then run ln
                  else putStrLn "Unknown command. Try: help"
    where cmd :: String
          cmd = head $ words ln
