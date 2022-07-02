module Main where

import System.IO (hSetBuffering, stdin, stdout, BufferMode(..))
import System.Environment (getArgs)

{- Four Mutable Things:
 - 1. Accumulator Register
 - 2. Location Counter
 - 3. Memory
 - 4. Symbol Table
-}

data Mnemon = HLT -- STOP
            | LOD -- Load Acc from Mem Location
            | STO -- Store Acc to Mem Location 
            | ADD -- Add Memory Location and Acc, and store in Acc
            | BZE -- Branch to Label if Acc == 0
            | BNE -- Branch to Label if Acc < 0
            | BRA -- Unconditional Branc
            | INP -- Set the next char in the input stream to Acc
            | OUT -- Output the next character (from Acc)
            | CLA -- Set Acc to 0
            deriving (Show, Eq, Bounded, Enum)

hlt, lod, sto, add, bze, bra, inp, out, cla :: Integer 
hlt = 0
lod = 1
sto = 2
add = 3
bze = 4
bne = 5
bra = 6
inp = 7
out = 8
cla = 9

menuTable :: [(String, String)]
menuTable = [ ("help", "Display help menu")
            , ("pacc", "Print accumulator register")
            , ("pmem", "Print memory location contents")
            , ("quit", "Quit the virtual machine")
            ]

help :: IO ()
help = do putStrLn "Possible commands are:\n"
          mapM_ (\x -> putStrLn ((fst x) ++ "\t\t" ++ (snd x))) menuTable 

runFiles :: [String] -> IO ()
runFiles args = mapM_ (\f -> putStrLn ("file: " ++ f)) args

runPrompt :: IO ()
runPrompt = do putStr "> "
               ln <- getLine
               (putStrLn ln) >> runPrompt

main :: IO ()
main = do hSetBuffering stdin NoBuffering
          args <- getArgs
          case args of
            [f1,f2] -> runFiles args
            []      -> runPrompt
