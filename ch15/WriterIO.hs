{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

import Control.Monad.Writer
import MonadHandle
import System.IO (IOMode(..))
import SafeHello

data Event = Open FilePath IOMode
           | Put String String
           | Close String
           | GetContents String
             deriving (Show)

newtype WriterIO a = W { runW :: Writer [Event] a }
    deriving (Functor, Applicative, Monad, MonadWriter [Event])

instance MonadHandle FilePath WriterIO where
    openFile path mode = tell [Open path mode] >> return path
    hPutStr h str = tell [Put h str]
    hClose h = tell [Close h]
    hGetContents h = tell [GetContents h] >> return ""

runWriterIO :: WriterIO a -> (a, [Event])
runWriterIO = runWriter . runW

-- ghci> :l WriterIO.hs SafeHello.hs MonadHandle.hs MonadHandleIO.hs
-- ghci> runWriterIO (safeHello "foo")