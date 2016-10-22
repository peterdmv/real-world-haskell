import Test.QuickCheck

data Ternary
    = Yes
    | No
    | Unknown
    deriving (Eq, Show)

instance Arbitrary Ternary where
  arbitrary = elements [Yes, No, Unknown]