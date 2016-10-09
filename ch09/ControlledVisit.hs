{-# LANGUAGE ScopedTypeVariables #-}
module ControlledVisit where

import Control.Exception (bracket, handle, IOException)
import Control.Monad (forM, liftM)
import Data.Time.Clock (UTCTime(..))
import Prelude hiding (traverse)
import System.Directory (Permissions(..), getDirectoryContents, getModificationTime, getPermissions)
import System.FilePath ((</>))
import System.IO (IOMode(..), hClose, hFileSize, openFile)

data Info = Info {
    infoPath :: FilePath
  , infoPerms :: Maybe Permissions
  , infoSize :: Maybe Integer
  , infoModTime :: Maybe UTCTime
  } deriving (Eq, Ord, Show)

getInfo :: FilePath -> IO Info
getInfo path = do
  perms <- maybeIO (getPermissions path)
  size <- maybeIO (bracket (openFile path ReadMode) hClose hFileSize)
  modified <- maybeIO (getModificationTime path)
  return (Info path perms size modified)

-- It can avoid recursing into directories, but it can't filter names until
-- the entire list of names in a tree has been generated.
--
traverse :: ([Info] -> [Info]) -> FilePath -> IO [Info]
traverse order path = do
  names <- getUsefulContents path
  contents <- mapM getInfo (path : map (path </>) names)
  liftM concat $ forM (order contents) $ \info -> do
    if isDirectory info && infoPath info /= path
       then traverse order (infoPath info)
       else return [info]

getUsefulContents :: FilePath -> IO [String]
getUsefulContents path = do
  names <- getDirectoryContents path
  return (filter (`notElem` [".", ".."]) names)

isDirectory :: Info -> Bool
isDirectory = maybe False searchable . infoPerms

maybeIO :: IO a -> IO (Maybe a)
maybeIO act = handle (\(_::IOException) -> return Nothing) (Just `liftM` act)

