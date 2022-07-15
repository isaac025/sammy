{-# LANGUAGE FlexibleContexts #-}

module Main where

import System.Environment (getArgs)
import System.Exit (exitSuccess)
import Data.Word (Word8)
import Data.IORef
import Control.Monad.IO.Class (liftIO)
import Reader

type Actual = Word8

type OpCode     = String
type Operand    = String
type Label      = String

newtype Asm = Asm { accumulator :: IORef Int }

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

maybeRead :: String -> Maybe Int
maybeRead s = case reads s of
    [(x, "")] -> Just x
    _         -> Nothing

go []       = pure $ exitSuccess
go (x:xs)   = case x of 
    WithLabel l _      -> go xs

    WithOperand opc op -> case any (== opc) opcodes of
        True    -> case maybeRead op of
            Just x -> do ac <- asks accumulator
                         liftIO $ modifyIORef ac (+x)
                         go xs
            Nothing -> go xs
        False   -> error "INSTRUCTION NOT SUPPORTED"

    WithoutOperand opc -> case any (== opc) opcodes of
        True    -> go xs
        False   -> error "INSTRUCTION NOT SUPPORTED"
    
parseFile :: String -> [ByteCode]
parseFile args = map toByteCode $ map words $ lines args

{-
 - State
 -
-}

main :: IO ()
main = do args <- getArgs
          accu <- newIORef 0
          let asm = Asm { accumulator = accu }
          case (length args) == 0 of
            True    -> putStrLn "Usage samasm: [FILE]"
            False   -> do contents <- readFile (head args)
                          runReader (go $ parseFile contents) asm
                          accu' <- readIORef accu
                          print accu'
