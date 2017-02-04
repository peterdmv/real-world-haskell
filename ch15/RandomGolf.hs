import Supply
import Control.Arrow (first)
import System.Random hiding (next)

randomsIO_golfed :: Random a => IO [a]
randomsIO_golfed = getStdRandom (first randoms . split)
