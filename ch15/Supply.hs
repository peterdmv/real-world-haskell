{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Supply
    (
      Supply
    , next
    , runSupply
    ) where

import Control.Monad.State

newtype Supply s a = S (State [s] a)
    deriving (Functor, Applicative, Monad)

runSupply :: Supply s a -> [s] -> (a, [s])
runSupply (S m) xs = runState m xs

next :: Supply s (Maybe s)
next = S $ do st <- get
              case st of
                [] -> return Nothing
                (x:xs) -> do put xs
                             return (Just x)

-- Desugarized version of next
--
nextNoDo :: Supply s (Maybe s)
nextNoDo = S $ let fun pattern =
                    case pattern of
                      []     -> return Nothing
                      (x:xs) -> put xs >> return (Just x)
               in get >>= fun

-- runSupply (liftM2 (,) next next) [1,2,3]
-- runSupply (liftM2 (liftM2 (,)) next next) [1,2,3]
--
-- (,) :: a -> b -> (a, b)
-- :: Supply s a -> Supply s b -> Supply s (a, b)
-- :: Supply s (Maybe s) -> Supply s (Maybe s) -> Supply s (Maybe s, Maybe s)
--
-- liftM2 f x y is equivalent to
-- liftM2 f m1 m2 = do
--   x1 <- m1
--   x2 <- m2
--   return (f x1 x2)

showTwo :: (Show s) => Supply s String
showTwo = do
  a <- next
  b <- next
  return ("a: " ++ show a ++ ", b: " ++ show b)

-- ghci> runSupply showTwo [1,2,3]
