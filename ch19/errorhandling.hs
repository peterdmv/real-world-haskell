import Control.Monad.Error

data ArithmeticError =
    DivideByZero
  | NotDivisible Int Int
  | OtherError String
  deriving Show

instance Error ArithmeticError where
  strMsg = OtherError

-- Division that checks for divide-by-zero and not-divisible errors.
safe_divide :: Int -> Int -> Either ArithmeticError Int
safe_divide _ 0 = throwError DivideByZero
safe_divide i j | i `mod` j /= 0 = throwError (NotDivisible i j)
safe_divide i j = return (i `div` j)

-- Division that checks for divide-by-zero errors but that
-- allows not-divisible conditions (throwing away the remainder).
divide :: Int -> Int -> Either ArithmeticError Int
divide i j = (i `safe_divide` j)
               `catchError` \e ->
                  case e of
                    -- Note: OtherErrors are just re-thrown.
                    OtherError s     -> throwError (OtherError s)
                    DivideByZero     -> throwError DivideByZero
                    NotDivisible i j -> return (i `div` j)