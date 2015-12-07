-- Requires Linear package for V2
module Day3 where
import Common
import Linear.V2
import Data.Set (fromList, union)

part1 content = length $ fromList (travel $ toVectors content)
part2 content = let
	(santa, roboSanta) = alternating (toVectors content)
	in length $ union (fromList $ travel santa) (fromList $ travel roboSanta)

toVec '^' = V2  0  1
toVec 'v' = V2  0 (-1)
toVec '<' = V2 (-1)  0
toVec '>' = V2  1  0

travel = scanl (+) (V2 0 0)
toVectors = Prelude.map toVec

-- Creates two alternating lists from one.
-- Example: [1,2,3,4,5] -> ([1,3,5], [2,4])
alternating list = let
	indexed = zipWithIndices list
	in (filterIndices odd indexed, filterIndices even indexed)

zipWithIndices list = zip list [0..]
filterIndices fn zippedIndices = map fst (filter (fn.snd) zippedIndices)
