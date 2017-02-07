import Control.Applicative (liftA2)
import Control.Monad (liftM2)
import Text.ParserCombinators.Parsec
import Numeric (readHex)

p_query :: CharParser () [(String, Maybe String)]
p_query = p_pair_app1 `sepBy` char '&'

p_hex :: CharParser () Char
p_hex = do
  char '%'
  a <- hexDigit
  b <- hexDigit
  let ((d, _):_) = readHex [a,b]
  return . toEnum $ d

p_char :: CharParser () Char
p_char = oneOf urlBaseChars
     <|> (char '+' >> return ' ')
     <|> p_hex

p_pair_app1 =
    liftM2 (,) (many1 p_char) (optionMaybe (char '=' >> many p_char))

urlBaseChars = ['a'..'z']++['A'..'Z']++['0'..'9']++"$-_.!*'(),"

-- Applicative Parsing
--

a_query :: CharParser () [(String, Maybe String)]
a_query = a_pair `sepBy` char '&'

a_hex :: CharParser () Char
a_hex = hexify <$> (char '%' *> hexDigit) <*> hexDigit
    where hexify a b = toEnum . fst . head . readHex $ [a,b]

hexify :: Char -> Char -> Char
hexify a b = toEnum . fst . head . readHex $ [a,b]

a_char = oneOf urlBaseChars
     <|> (' ' <$ char '+')
     <|> a_hex

a_pair :: CharParser () (String, Maybe String)
a_pair = liftA2 (,) (many1 a_char) (optionMaybe (char '=' *> many a_char))
