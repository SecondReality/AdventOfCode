module Day13 where
import Common
import Data.List
import qualified Data.Map as M
import Text.Regex.PCRE

-- Note that this is almost exactly the same as day 9.
-- The differences being:
--  The person first on the list is copied and added to the back of the list (since a table is circular)
--  Instead of a set used to lookup distances, a tuple is used since order is now important
--  The distance between two people is calculated as d(a,b) + d(b,a)

part1 content = greatestDistance places distanceStore
  where (places, distanceStore) = placesAndDistanceStore content

part2 content = greatestDistance ("me":places) distanceStore
  where (places, distanceStore) = placesAndDistanceStore content

greatestDistance places distanceStore = maximum allDistances
  where allPermutations = permutations places -- this actually returns too many permutations.
        tabledPermutation = map (\ x -> x ++ take 1 x ) allPermutations
        allDistances = map (totalDistance distanceStore) tabledPermutation

placesAndDistanceStore content = (places, distanceStore)
  where parseContent = map parse $ lines content
        places = allPlaces parseContent
        distanceStore = getDistanceStore parseContent

-- Gets the distance between two places. Any comparison involving me returns 0 (for part2).
getDistance distanceStore ("me", _) = 0
getDistance distanceStore (_, "me") = 0
getDistance distanceStore (p1, p2) = distanceStore M.! (p1, p2) + distanceStore M.! (p2, p1)

-- Gets the total distance travelled along a list of places
totalDistance distanceStore places = sum $ map (getDistance distanceStore) $ getPairs places

-- Gets all the place names given the parse content
allPlaces parseContent = nub . concat $ map (\ (p1,p2,d) -> [p1,p2]) parseContent

-- Gets a Map that contains the distances between placenames
-- where the key is a pair of placenames (in a set) and the value their distance apart
getDistanceStore :: [(String, String, Int)] -> M.Map (String, String) Int
getDistanceStore parseContent = M.fromList $ map (\ (p1,p2,d) -> ((p1, p2), d)) parseContent

-- Parses a line of the file and returns the placenames and distance apart
parse :: String -> (String, String, Int)
parse str = (person1, person2, signHappiness gainOrLose (read happiness :: Int))
  where (_,_,_, [person1, gainOrLose, happiness, person2]) = (str =~ "(.+) +would +(gain|lose) +(\\d+) +happiness units by sitting next to +(.+)\\.") :: (String,String,String, [String])
        signHappiness "gain" happinessVal = happinessVal
        signHappiness "lose" happinessVal = -happinessVal

-- Given a list, returns all the pairs as you travel along the list
-- [1,2,3,4] -> [(1, 2), (2, 3), (3, 4)]
getPairs list = zip list $ tail list
