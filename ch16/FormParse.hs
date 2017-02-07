import Control.Monad (liftM2)
import Numeric (readHex)
import Text.ParserCombinators.Parsec

p_query :: CharParser () [(String, Maybe String)]
p_query = p_pair `sepBy` char '&'

p_pair :: CharParser () (String, Maybe String)
p_pair = do
  name <- many1 p_char
  -- optionMaybe p tries to apply parser p. If p fails without consuming
  -- input, it return Nothing, otherwise it returns Just the value
  -- returned by p.
  -- optionMaybe can fail if the parser has consumed input
  -- ghci> parse (optionMaybe (char '3' >> many1 (char '3'))) "" "3"
  value <- optionMaybe (char '=' >> many p_char)
  return (name, value)

p_pair_app1 =
    liftM2 (,) (many1 p_char) (optionMaybe (char '=' >> many p_char))

p_char :: CharParser () Char
p_char = oneOf urlBaseChars
     <|> (char '+' >> return ' ')
     <|> p_hex

urlBaseChars = ['a'..'z']++['A'..'Z']++['0'..'9']++"$-_.!*'(),"

p_hex :: CharParser () Char
p_hex = do
  char '%'
  a <- hexDigit
  b <- hexDigit
  let ((d, _):_) = readHex [a,b]
  return . toEnum $ d
