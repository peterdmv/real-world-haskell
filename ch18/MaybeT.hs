{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module MaybeT where

import Control.Monad (ap, liftM)
import Control.Monad.State
import Control.Monad.Trans
import Control.Monad.Writer

newtype MaybeT m a = MaybeT {
      runMaybeT :: m (Maybe a)
    }

bindMT :: (Monad m) => MaybeT m a -> (a -> MaybeT m b) -> MaybeT m b
x `bindMT` f = MaybeT $ do
                 unwrapped <- runMaybeT x
                 case unwrapped of
                   Nothing -> return Nothing
                   Just y -> runMaybeT (f y)

x `altBindMT` f =
    MaybeT $ runMaybeT x >>= maybe (return Nothing) (runMaybeT . f)

returnMT :: (Monad m) => a -> MaybeT m a
returnMT a = MaybeT $ return (Just a)

failMT :: (Monad m) => t -> MaybeT m a
failMT _ = MaybeT $ return Nothing

instance (Monad m) => Functor (MaybeT m) where
  fmap = liftM

instance (Monad m) => Applicative (MaybeT m) where
  pure = returnMT
  (<*>) = ap

instance (Monad m) => Monad (MaybeT m) where
  return = returnMT
  (>>=) = bindMT
  fail = failMT

instance MonadTrans MaybeT where
    lift m = MaybeT (Just `liftM` m)

instance (MonadIO m) => MonadIO (MaybeT m) where
  liftIO m = lift (liftIO m)

instance (MonadState s m) => MonadState s (MaybeT m) where
  get = lift get
  put k = lift (put k)

instance (Monoid w, MonadWriter w m) => MonadWriter w (MaybeT m) where
  tell = lift . tell
  listen m = MaybeT $ do
               (result,log) <- listen (runMaybeT m)
               case result of
                   Nothing -> return Nothing
                   Just value -> return (Just (value,log))
  pass m = MaybeT $ do
             result <- runMaybeT m
             case result of
               Nothing -> return Nothing
               Just (value,log) -> pass (return (Just value,log))

-- ... and so on for MonadReader, etc ...

