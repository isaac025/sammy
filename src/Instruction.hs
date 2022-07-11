module Instruction where

import Data.Word (Word8)

type Addr           = Word8
type Val            = Word8    
type Op             = Word8
type Accumulator    = IORef Val

data Mnemonic = HLT -- STOP
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

--newtype Memory = Memory { mem :: Vector Word8 } 

-- Operations
lod :: Val -> Val -> Val
lod acu memVal = undefined -- stores memVal in acu

sto :: Val -> Addr -> Val
sto acu memAddr = undefined -- stores acu in memAddr

add :: Val -> Val -> Val
add acu memVal = undefined -- sums acu & memVal, and stores it in acu

bze :: Val -> Op -> Bool
bze acu op = undefined -- goto op if acu == 0

bne :: Val -> Op -> Bool
bne acu op = undefined -- goto op if acu < 0

bra :: Op -> Bool
bra op = undefined -- goto op unconditionally

inp :: Char -> Val
inp acu = undefined -- store next char in acu

out :: Val -> Char
out acu = undefined -- output current char in acu

cla :: Val -> Val
cla acu = undefined -- set acu to 0 (clear acu)

hlt :: ()
hlt = undefined -- halt the program


