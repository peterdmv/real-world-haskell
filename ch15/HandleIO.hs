{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module HandleIO
    ( HandleIO
    , Handle
    , IOMode(..)
    , runHandleIO
    , openFile
    , hClose
    , hPutStrLn
    ) where

import System.IO (Handle, IOMode(..))
import qualified System.IO
import Control.Monad.Trans (MonadIO(..))
import System.Directory (removeFile)

newtype HandleIO a = HandleIO { runHandleIO :: IO a }
    deriving (Functor, Applicative, Monad)

instance MonadIO HandleIO where liftIO = HandleIO

openFile :: FilePath -> IOMode -> HandleIO Handle
openFile path mode = HandleIO (System.IO.openFile path mode)

hClose :: Handle -> HandleIO ()
hClose = HandleIO . System.IO.hClose

hPutStrLn :: Handle -> String -> HandleIO ()
hPutStrLn h s = HandleIO (System.IO.hPutStrLn h s)

safeHello :: FilePath -> HandleIO ()
safeHello path = do
  h <- openFile path WriteMode
  hPutStrLn h "hello world"
  hClose h

tidyHello :: FilePath -> HandleIO ()
tidyHello path = do
  safeHello path
  liftIO (removeFile path)
