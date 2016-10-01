-- IO with no buffering
--
import Data.Char(toUpper)
import System.IO(hSetBuffering, stdin, BufferMode(NoBuffering))

main = do
       hSetBuffering stdin NoBuffering
       interact (map toUpper)