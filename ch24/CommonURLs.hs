module Main where

import Control.Parallel.Strategies (NFData(..), rseq)
import Control.Monad (forM_)
import Data.Either.Unwrap (fromRight)
import Data.List (foldl', sortBy)
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.ByteString.Char8 as S
import qualified Data.Map as M
import PCRECompile (compile)
import RegexExec (match)

import System.Environment (getArgs)
import System.IO
import LineChunks (chunkedReadWith)
import MapReduce (mapReduce)

{-
rwhnf renamed to rseq

TODO: fix main function

sudo apt install libpcre2-dev
stack ghci --ghci-options -lpcre
-}

countURLs :: [L.ByteString] -> M.Map S.ByteString Int
countURLs = mapReduce rseq (foldl' augment M.empty . L.lines)
                      rseq (M.unionsWith (+))
  where augment map line =
            case match (fromRight (compile pattern [])) (strict line) [] of
              Just (_:url:_) -> M.insertWith' (+) url 1 map
              _ -> map
        strict  = S.concat . L.toChunks
        pattern = S.pack "\"(?:GET|POST|HEAD) ([^ ]+) HTTP/"

main :: IO ()
main = do
  args <- getArgs
  forM_ args $ \path -> do
    h <- openFile path ReadMode
    contents <- L.hGetContents h
    --putStrLn $ path ++ ": " ++ show $ countURLs [contents]
    return ()
