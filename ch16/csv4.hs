import Text.ParserCombinators.Parsec

-- This function is not correct!
eol :: GenParser Char st [Char]
eol = string "\n\r" <|> string "\n"