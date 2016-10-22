module QC where

import Control.Monad (liftM, liftM2)
import Data.List (intersperse)
import Test.QuickCheck
import Test.QuickCheck.Arbitrary


data Doc = Empty
         | Char Char
         | Text String
         | Line
         | Concat Doc Doc
         | Union Doc Doc
         deriving (Show,Eq)

empty :: Doc
empty = Empty

char :: Char -> Doc
char c = Char c

text :: String -> Doc
text "" = Empty
text s  = Text s

double :: Double -> Doc
double d = text (show d)

line :: Doc
line = Line

(<>) :: Doc -> Doc -> Doc
Empty <> y = y
x <> Empty = x
x <> y = x `Concat` y


hcat :: [Doc] -> Doc
hcat = fold (<>)

fold :: (Doc -> Doc -> Doc) -> [Doc] -> Doc
fold f = foldr f empty

fsep :: [Doc] -> Doc
fsep = fold (</>)

(</>) :: Doc -> Doc -> Doc
x </> y = x <> softline <> y

softline :: Doc
softline = group line

group :: Doc -> Doc
group x = flatten x `Union` x

flatten :: Doc -> Doc
flatten (x `Concat` y) = flatten x `Concat` flatten y
flatten Line = Char ' '
flatten (x `Union` _) = flatten x
flatten other = other

punctuate :: Doc -> [Doc] -> [Doc]
punctuate p []     = []
punctuate p [d]    = [d]
punctuate p (d:ds) = (d <> p) : punctuate p ds

compact :: Doc -> String
compact x = transform [x]
  where transform [] = ""
        transform (d:ds) = case d of
          Empty     -> transform ds
          Char c    -> c : transform ds
          Text s    -> s ++ transform ds
          Line      -> '\n' : transform ds
          a `Concat` b -> transform (a:b:ds)
          _ `Union` b  -> transform (b:ds)

pretty :: Int -> Doc -> String
pretty width x = best 0 [x]
  where best col (d:ds) = case d of
          Empty        -> best col ds
          Char c       -> c : best (col +1) ds
          Text s       -> s ++ best (col + length s) ds
          Line         -> '\n' : best 0 ds
          a `Concat` b -> best col (a:b:ds)
          a `Union` b  -> nicest col (best col (a:ds))
                                     (best col (b:ds))
        best _ _ = ""

        nicest col a b | (width - least) `fits` a = a
                       | otherwise                = b
                       where least = min width col

fits :: Int -> String -> Bool
w `fits` _ | w < 0 = False
w `fits` ""        = True
w `fits` ('\n':_)  = True
w `fits` (c:cs)    = (w - 1) `fits` cs


-- QuickCheck already includes an instance
--
-- instance Arbitrary Char where
--     arbitrary = elements (['A'..'Z'] ++ ['a'..'z'] ++ " ~!@#$%^&*()")

-- instance Arbitrary Doc where
--     arbitrary = do
--         n <- choose (1,6) :: Gen Int
--         case n of
--              1 -> return Empty
--              2 -> do x <- arbitrary
--                      return (Char x)
--              3 -> do x <- arbitrary
--                      return (Text x)
--              4 -> return Line
--              5 -> do x <- arbitrary
--                      y <- arbitrary
--                      return (Concat x y)
--              6 -> do x <- arbitrary
--                      y <- arbitrary
--                      return (Union x y)

instance Arbitrary Doc where
    arbitrary =
        oneof [ return Empty
              , liftM Char arbitrary
              , liftM Text arbitrary
              , return Line
              , liftM2 Concat arbitrary arbitrary
              , liftM2 Union arbitrary arbitrary ]

prop_empty_id x =
    empty <> x == x
  &&
    x <> empty == x

prop_char c = char c == Char c

prop_text s = text s == if null s then Empty else Text s

prop_line = line == Line

prop_double d = double d == text (show d)


prop_hcat xs = hcat xs == glue xs
    where
        glue [] = Empty
        glue (d:ds) = d <> glue ds

prop_punctuate s xs = punctuate s xs == intersperse s xs

prop_punctuate' s xs = punctuate s xs == combine (intersperse s xs)
    where
        combine []           = []
        combine [x]          = [x]
        combine (x:Empty:ys) = x : combine ys
        combine (Empty:y:ys) = y : combine ys
        combine (x:y:ys)     = x `Concat` y : combine ys