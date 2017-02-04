module SafeHello where

import MonadHandle
import MonadHandleIO
import System.IO (IOMode(..))

safeHello :: MonadHandle h m => FilePath -> m ()
safeHello path = do
    h <- openFile path WriteMode
    hPutStrLn h "hello world"
    hClose h

-- ghci>:l SafeHello.hs MonadHandle.hs MonadHandleIO.hs
