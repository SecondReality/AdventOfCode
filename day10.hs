-- Requires QuickCheck and doctest for unit tests.
module Day10 where
import Common
import Data.List

part1 content = length $ elfRecursion content 40
part2 content = length $ elfRecursion content 50

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
-- prop> \xs -> if length xs > 0 then elem (head xs) (elfIteration xs) else True

elfIteration str = concatMap (\ xs -> show (length xs)++[head xs]) $ group str

-- | Computes multiple iterations of the elf game and returns the resulting string
--
-- Examples:
--
-- >>> elfRecursion "1" 1
-- "11"
--
-- >>> elfRecursion "1" 2
-- "21"
--
-- prop> \xs -> elfRecursion xs 1 == elfIteration xs
-- prop> \xs -> elfRecursion xs 2 == elfIteration (elfIteration xs)

elfRecursion str iterations = foldl (\ x _-> elfIteration x) str [1..iterations]
