module Day11 where
import Common
import Data.List
import Data.Char

part1 str = until part1Valid next $ next str

next [] = "a"
next str = if backInc > 'z' then next front++"a" else front++[backInc]
  where backInc = nextChar $ last str
        front = init str

nextChar c = chr $ ord c + 1

part1Valid str = all ($ str) [requirement1, requirement2, requirement3]

charSet = ['a'..'z'] \\ "iol"

requirement1 str = any incrementing $ toGroups str 3
requirement2 str = 0 >= length (intersect "iol" str)
requirement3 str = any duplicates $ tails $ toGroups str 2

incrementing str = y == x+1 && z == y+1
                          where [x,y,z] = map ord str

duplicates (front:_:back) = isPair front && any isPair back
duplicates _ = False

isPair [x, y] = x==y
