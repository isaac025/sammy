{-# LANGUAGE GADTs #-}

module VMPrinter where

import Control.Monad.Freer
import Control.Monad.Freer.Internal

data MemoryPrinter s where
    PutStrLn :: String -> MemoryPrinter ()
