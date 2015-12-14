module Day8 where
import Common
import Data.Char

part1 content = sum $ map processCount (lines content)

removeQuotes = init.tail

lengthOf str len = if length str >= len then Just (take len str) else Nothing

isHex str = take 2 str == "\\x" && all isHexDigit (drop 2 str)
isBackslash str = str == "\\\\"
isQuote str = str == "\\\""

processCount str = length str - tokenize (removeQuotes str)

tokenize "" = 0
tokenize str
  | backslashMatch || quoteMatch = 1 + tokenize (drop 2 str)
  | hexMatch = 1 + tokenize (drop 4 str)
  | otherwise = 1 + tokenize (drop 1 str)
  where hexMatch = maybe False isHex $ lengthOf str 4
        backslashMatch = maybe False isBackslash $ lengthOf str 2
        quoteMatch = maybe False isQuote $ lengthOf str 2


