{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Control.Monad.Except
import Control.Monad.State
import qualified Data.ByteString.Char8 as B

import Data.Char (isDigit)

data ParseError = NumericOverflow
                | EndOfInput
                | Chatty String
                  deriving (Eq, Ord, Show)

newtype Parser a = P {
      runP :: ExceptT ParseError (State B.ByteString) a
    } deriving (Functor, Applicative, Monad, MonadError ParseError)

liftP :: State B.ByteString a -> Parser a
liftP m = P (lift m)

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = do
  s <- liftP get
  case B.uncons s of
    Nothing         -> throwError EndOfInput
    Just (c, s')
        | p c       -> liftP (put s') >> return c
        | otherwise -> throwError (Chatty "satisfy failed")

optional :: Parser a -> Parser (Maybe a)
optional p = (Just `liftM` p) `catchError` \_ -> return Nothing

runParser :: Parser a -> B.ByteString
          -> Either ParseError (a, B.ByteString)
runParser p bs = case runState (runExceptT (runP p)) bs of
                   (Left err, _) -> Left err
                   (Right r, bs) -> Right (r, bs)