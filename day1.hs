module Day1 where
import Common
import Data.List

part1 content = sum $ toNumbers content
part2 content = elemIndex (-1) $ travel (toNumbers content)

toNumbers = map (\x -> if x=='(' then 1 else -1 )
travel = scanl (+) 0
