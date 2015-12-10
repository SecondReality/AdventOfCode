module Day10 where
import Common
import Data.List

part1 content = elfRecursion content 0 40
part2 content = elfRecursion content 0 50

-- | Computes a single iteration of the elf game
--
-- Examples:
--
-- >>> elfIteration "1"
-- "11"
--
-- >>> elfIteration "11"
-- "21"
--
-- >>> elfIteration "111221"
-- "312211"
--
-- The following line will pass in random strings to elfIteration and verify the resulting string is the same length of longer.
-- prop> \xs -> length(elfIteration xs)>= (length xs)

elfIteration str = concatMap (\ xs -> show (length xs)++[head xs]) $ group str

-- | Computes multiple iterations of the elf game and returns the length of the resulting string
--
-- Examples:
--
-- >>> elfRecursion "1" 0 1
-- 2
--
-- >>> elfRecursion "1" 0 2
-- 2
--
-- >>> elfRecursion "3113322113" 0 40
-- 329356

elfRecursion str depth maxDepth | depth == maxDepth = length str
                                | otherwise = elfRecursion (elfIteration str) (depth+1) maxDepth
