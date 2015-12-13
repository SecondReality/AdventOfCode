module Common where
import Data.List

fromInput x fileName= do
  content <- readFile fileName
  print (x content)

count elem list = length $ filter (==elem) list

-- Groups a list into overlapping strings of given length. Every entry with be of the given length.
-- example: toGroups "Hello" 2 -> ["He","el","ll","lo"]
toGroups str len = filter (\x -> len==length x) (map (take len) (tails str))
