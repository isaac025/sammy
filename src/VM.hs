module VM where

import Data.Word (Word8)
import Data.IORef
import Text.Printf (printf)
import Data.Bits ((.&.), shiftR)

type Addr   = Word8
type Val    = Word8

newtype Accumulator = Accumulator { acc :: IORef Word8 
                                  } 

newtype Memory = Memory { mem :: [Word8]
                        }

x :: Word8
x = 0x2A

--The last 3 bits of the integer are:
f = x .&. 0x7

--The five bits starting from the eight-last bit are:
f' = x `shiftR` 3    -- all but the last three bits .&.  0x1F -- the last five bits.

