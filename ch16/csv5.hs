import Text.ParserCombinators.Parsec

-- This function is not correct!
eol :: GenParser Char st Char
eol =
    do char '\n'
       char '\r' <|> return '\n'