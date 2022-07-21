{-# LANGUAGE FlexibleContexts #-}

module Main where

import System.Environment (getArgs)
import Data.Word (Word8)
import Data.IORef
import VMPrinter (runPrompt)

type OpCode     = String
type Operand    = String
type Label      = String

newtype Asm = Asm { accumulator :: IORef Word8 }

opcodes :: [String]
opcodes = [ "HLT" -- STOP
          , "LOD" -- Load Acc from Mem Location
          , "STO" -- Store Acc to Mem Location 
          , "ADD" -- Add Memory Location and Acc, and store in Acc
          , "BZE" -- Branch to Label if Acc == 0
          , "BNE" -- Branch to Label if Acc < 0
          , "BRA" -- Unconditional Branc
          , "INP" -- Set the next char in the input stream to Acc
          , "OUT" -- Output the next character (from Acc)
          , "CLA" -- Set Acc to 0
          ] 

data ByteCode = WithLabel Label ByteCode
              | WithOperand OpCode Operand
              | WithoutOperand OpCode 
              deriving (Show, Eq)

toByteCode :: [String] -> ByteCode
toByteCode x
    | length x == 1 = WithoutOperand (head x)
    | length x == 2 = WithOperand (head x) (head $ tail x)
    | otherwise     = WithLabel (head x) (toByteCode (tail x))

printInstr :: OpCode -> IO ()
printInstr "HLT" = putStrLn "Program Halted!" 
printInstr "LOD" = putStrLn "Load to accumulator" 
printInstr "STO" = putStrLn "Store accumulator to memory" 
printInstr "ADD" = putStrLn "Adding" 
printInstr "BZE" = putStrLn "Branch Zero Equal" 
printInstr "BNE" = putStrLn "Branch Negative" 
printInstr "BRA" = putStrLn "Branch Unconditional" 
printInstr "INP" = putStrLn "Input" 
printInstr "OUT" = putStrLn "Output" 
printInstr "CLA" = putStrLn "Clear accumulator"
printInstr x = putStrLn x

parseFile :: String -> [ByteCode]
parseFile args = map (toByteCode . words) $ lines args

main :: IO ()
main = do args <- getArgs
          accu <- newIORef 0
          let asm = Asm { accumulator = accu }
          if null args then runPrompt
          else  do contents <- readFile (head args)
                   print "Something"
