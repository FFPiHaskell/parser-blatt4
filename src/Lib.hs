module Lib 
        ( parseCSV
        , CSV
        , Row
        , Field
        )
       where

import Data.Attoparsec.Text as A
import Data.Text as T (pack, strip, Text(..), null)
import Control.Applicative
import Control.Monad (void, unless, when)
import Debug.Trace (trace)


-- csv-file       = { row }
-- row            = field-list, eol
-- field-list     = field, [ ",", field-list ]
-- field          = [ whitespace ], field-value, [ whitespace ]
-- field-value    = quoted-string | bare-string
-- quoted-string  = '"', quoted-content, '"'
-- quoted-content = { quoted-char }
-- quoted-char    = (any char except '"' or eol)
-- bare-string    = { bare-char }
-- bare-char      = (any char except ',' or eol without whitespace at beginning/end)
-- whitespace     = space-char, { space-char }
-- space-char     = " " | "\t"
-- eol            = "\n"


type CSV = [Row]
type Row = [Field]
type Field = Text


parseCSV :: String -> Either String CSV
parseCSV s = parseOnly csvParser (pack s)

csvParser :: Parser CSV
csvParser = many rowParser

rowParser :: Parser Row
rowParser = do
             e <- atEnd
             when e $ fail "no more rows"
             f <- fieldParser
             fs <- many (char ',' *> fieldParser)
             endOfLine <|> endOfInput
             return (f:fs)

fieldParser :: Parser Field
fieldParser = do
              whitespace
              fv <- fieldValueParser
              whitespace
              return fv

whitespace :: Parser Text
whitespace = A.takeWhile pred
  where
          pred :: Char -> Bool
          pred ' '  = True
          pred '\t' = True
          pred _    = False


fieldValueParser :: Parser Text
fieldValueParser = quotedString <|> bareString
  where
          quotedString :: Parser Text
          quotedString = char '"' *> A.takeWhile quotedChar <* char '"'
          bareString :: Parser Text
          bareString = do
                         bc <- A.takeWhile bareChar
                         return $ strip bc
          quotedChar :: Char -> Bool
          quotedChar a
             | a == '"'      = False
             | isEndOfLine a = False
             | otherwise     = True
          bareChar :: Char -> Bool
          bareChar a
             | a == ','      = False
             | isEndOfLine a = False
             | otherwise     = True


