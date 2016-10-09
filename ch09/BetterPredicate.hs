{-# LANGUAGE ScopedTypeVariables #-}

import Control.Monad (filterM)
import System.Directory (Permissions(..), getModificationTime, getPermissions)
import Data.Time.Clock (UTCTime(..))
import System.FilePath (takeExtension)
import Control.Exception (bracket, handle, IOException)
import System.IO (IOMode(..), hClose, hFileSize, openFile)

import RecursiveContents (getRecursiveContents)

type Predicate = FilePath      -- path to directory entry
              -> Permissions   -- permissions
              -> Maybe Integer -- file size (Nothing if not file)
              -> UTCTime       -- last modified
              -> Bool

-- Throws an exception if an entry is not a plain file or could not
-- be opened, and returns the size unwrapped.
--
simpleFileSize :: FilePath -> IO Integer
simpleFileSize path = do
  h <- openFile path ReadMode
  size <- hFileSize h
  hClose h
  return size

-- Won't throw any exceptions. Exceptions will be caught by handle, but
-- our call to hClose will never occur. It could exhaust the supply of
-- open file handles before enough garbage file handles could be closed
-- and thus crash our program.
--
saferFileSize :: FilePath -> IO (Maybe Integer)
saferFileSize path = handle (\(_::IOException) -> return Nothing) $ do
  h <- openFile path ReadMode
  size <- hFileSize h
  hClose h
  return (Just size)

-- Completely safe variant
--
getFileSize :: FilePath -> IO (Maybe Integer)
getFileSize path = handle (\(_::IOException) -> return Nothing) $
  bracket (openFile path ReadMode) hClose $ \h -> do
    size <- hFileSize h
    return (Just size)


betterFind :: Predicate -> FilePath -> IO [FilePath]
betterFind p path = getRecursiveContents path >>= filterM check
  where check name = do
          perms <- getPermissions name
          size <- getFileSize name
          modified <- getModificationTime name
          return (p name perms size modified)


type InfoP a = FilePath      -- path to directory entry
            -> Permissions   -- permissions
            -> Maybe Integer -- file size (Nothing if not file)
            -> UTCTime       -- last modified
            -> a

pathP :: InfoP FilePath
pathP path _ _ _ = path

sizeP :: InfoP Integer
sizeP _ _ (Just size) _ = size
sizeP _ _ Nothing _ = -1

equalP :: (Eq a) => InfoP a -> a -> InfoP Bool
equalP f k = \w x y z -> f w x y z == k

equalP' :: (Eq a) => InfoP a -> a -> InfoP Bool
equalP' f k w x y z = f w x y z == k

liftP :: (a -> b -> c) -> InfoP a -> b -> InfoP c
liftP q f k w x y z = f w x y z `q` k

greaterP, lesserP :: (Ord a) => InfoP a -> a -> InfoP Bool
greaterP = liftP (>)
lesserP = liftP (<)

simpleAndP :: InfoP Bool -> InfoP Bool -> InfoP Bool
simpleAndP f g w x y z = f w x y z && g w x y z

liftP2 :: (a -> b -> c) -> InfoP a -> InfoP b -> InfoP c
liftP2 q f g w x y z = f w x y z `q` g w x y z

andP = liftP2 (&&)
orP = liftP2 (||)

-- liftP in terms of liftP2
--
constP :: a -> InfoP a
constP k _ _ _ _ = k

liftP' :: (a -> b -> c) -> InfoP a -> b -> InfoP c
liftP' q f k w x y z = f w x y z `q` constP k w x y z

liftP'' :: (a -> b -> c) -> InfoP a -> b -> InfoP c
liftP'' q f k w x y z = liftP2 q f (constP k) w x y z


liftPath :: (FilePath -> a) -> InfoP a
liftPath f w _ _ _ = f w


myTest path _ (Just size) _ =
  takeExtension path == ".cpp" && size > 131072
myTest _ _ _ _ = False


myTest2 = (liftPath takeExtension `equalP` ".cpp") `andP`
          (sizeP `greaterP` 131072)

-- Defining new operators
--
(==?) = equalP
(&&?) = andP
(>?) = greaterP

infix 4 ==?
infixr 3 &&?
infix 4 >?

myTest3 = (liftPath takeExtension ==? ".cpp") &&? (sizeP >? 131072)
myTest4 = liftPath takeExtension ==? ".cpp" &&? sizeP >? 131072



