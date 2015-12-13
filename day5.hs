module Day5 where
import Common
import Prelude
import Data.List

part1 = countTrueLines isNice
part2 = countTrueLines isNicePart2

-- Breaks input into lines, applies a function to each and counts how many return true.
countTrueLines fn content = count True (map fn (lines content))

isNice str= all ($ str) [threeOrMoreVowels, hasConsecutiveLetters, doesntContainBadStrings]

threeOrMoreVowels str = length (filter (`elem` "aeiou") str) >= 3
compress = map head . group
hasConsecutiveLetters str = length (compress str) < length str
doesntContainBadStrings str = not.any  (`isInfixOf` str) $ ["ab", "cd", "pq", "xy"]

-- part 2 --
isNicePart2 content = all ($ content) [requirement1, requirement2]

requirement1 str = any duplicates (tailPairs str)
requirement2 str = any (\ x -> head x == last x) (toGroups str 3)

-- the tails of the pairs:
tailPairs str = tails $ toGroups str 2
duplicates pairs = elem (head pairs) (drop 2 pairs)
