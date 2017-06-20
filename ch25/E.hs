import System.Environment
import Text.Printf
import Control.Parallel.Strategies (NFData, rdeepseq, using)

main = do
    [d] <- map read `fmap` getArgs
    printf "%f\n" (mean [1..d])

foldl'rnf :: NFData a => (a -> b -> a) -> a -> [b] -> a
foldl'rnf f z xs = lgo z xs
    where
        lgo z []     = z
        lgo z (x:xs) = lgo z' xs
            where
                z' = f z x `using` rdeepseq

mean :: [Double] -> Double
mean xs = s / fromIntegral n
  where
    (n, s)     = foldl'rnf k (0, 0) xs
    k (n, s) x = (n+1, s+x) :: (Int, Double)

{-
  236 MB total memory in use (0 MB lost due to fragmentation)
  Productivity  23.2% of total user, 21.3% of total elapsed
-}
