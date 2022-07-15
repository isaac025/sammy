module VM where

import System.IO (hSetBuffering, stdin, stdout, BufferMode(..))
import System.Environment (getArgs)
import Data.Word (Word8)

{- Four Mutable Things:
 - 1. Location Counter
 - 2. Accumulator Register
 - 3. Memory
 - 4. Symbol Table
-}

runPrompt :: IO ()
runPrompt = do putStr "SAMMY> "
               ln <- getLine
               (putStrLn ln) >> runPrompt



menuTable :: [(String, String)]
menuTable = [ ("help", "Display help menu")
            , ("pacc", "Print accumulator register")
            , ("pmem", "Print memory location contents")
            , ("quit", "Quit the virtual machine")
            ]

help :: IO ()
help = do putStrLn "Possible commands are:\n"
          mapM_ (\x -> putStrLn ((fst x) ++ "\t\t" ++ (snd x))) menuTable 



