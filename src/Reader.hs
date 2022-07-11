{-# LANGUAGE BlockArguments #-}

module Reader where

import Control.Monad.IO.Class

newtype Reader r m a = Reader { runReader :: r -> m a 
                              }

instance (Functor m) => Functor (Reader r m) where
    fmap f = mapReader (fmap f)

instance (Applicative m) => Applicative (Reader r m) where
    pure = lift . pure
    f <*> v = Reader $ \ r -> runReader f r <*> runReader v r

instance (Monad m) => Monad (Reader r m) where
    return = lift . return
    m >>= k = Reader $ \r -> do a <- runReader m r
                                runReader (k a) r

instance MonadIO m => MonadIO (Reader r m) where
    liftIO = lift . liftIO 

lift :: m a -> Reader b m a
lift f = Reader (const f)

mapReader :: (m1 a1 -> m2 a2) -> Reader r m1 a1 -> Reader r m2 a2
mapReader f m = Reader $ f . runReader m

ask :: Monad m => Reader r m r
ask = Reader return

asks :: (Monad m) => (r -> a) -> Reader r m a
asks f = f <$> ask

withReader  :: (r -> s) -> Reader s m a -> Reader r m a
withReader m reader  = Reader (\env -> runReader reader (m env))

local       :: (r -> r) -> Reader r m a -> Reader r m a
local f r       = Reader $ runReader r . f 
