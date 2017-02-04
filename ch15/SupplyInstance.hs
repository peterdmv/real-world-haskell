{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import SupplyClass
import System.Random hiding (next)

-- The Reader Monad
--

newtype Reader e a = R { runReader :: e -> a }

instance Functor (Reader e) where
  fmap f m = R $ \r -> f (runReader m r)

instance Applicative (Reader e) where
  pure = R . const
  f <*> x = R $ \r -> runReader f r (runReader x r)

instance Monad (Reader e) where
  return a = R $ \_ -> a
  m >>= k = R $ \r -> runReader (k (runReader m r)) r

ask :: Reader e e
ask = R id

-- ghci> runReader (ask >>= \x -> return (x * 3)) 2


newtype MySupply e a = MySupply { runMySupply :: Reader e a }
    deriving (Functor, Applicative, Monad)

instance MonadSupply e (MySupply e) where
    next = MySupply $ do
             v <- ask
             return (Just v)
    -- more concise:
    -- next = MySupply (Just `liftM` ask)

xy :: (Num s, MonadSupply s m) => m s
xy = do
  Just x <- next
  Just y <- next
  return (x * y)

randomsIO :: Random a => IO [a]
randomsIO =
    getStdRandom $ \g ->
        let (a, b) = split g
        in (randoms a, b)

-- ghci> (fst . runSupply xy) `fmap` randomsIO
-- ghci> (fst . runSupply xy) `fmap` return [6,2,3]

runMS :: MySupply i a -> i -> a
runMS = runReader . runMySupply
