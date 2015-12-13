module Day9 where
import Common
import Data.List as L
import Data.Map as M
import Data.Set as S
import Text.Regex.PCRE

part1 content = maximum allDistances
  where parseContent = L.map parse $ lines content
        places = allPlaces parseContent
        distanceStore = getDistanceStore parseContent
        allPermutations = permutations places -- this actually returns too many permutations.
        allDistances = L.map (totalDistance distanceStore) allPermutations

-- Gets the distance between two places
getDistance distanceStore (p1, p2) = distanceStore ! S.fromList [p1,p2]

-- Gets the total distance travelled along a list of places
totalDistance distanceStore places = sum $ L.map (getDistance distanceStore) $ getPairs places

-- Gets all the place names given the parse content
allPlaces parseContent = nub . concat $ L.map (\ (p1,p2,d) -> [p1,p2]) parseContent

-- Gets a Map that contains the distances between placenames
-- where the key is a pair of placenames (in a set) and the value their distance apart
getDistanceStore :: [(String, String, Int)] -> Map (Set String) Int
getDistanceStore parseContent = M.fromList $ L.map (\ (p1,p2,d) -> (S.fromList [p1,p2], d)) parseContent

-- Parses a line of the file and returns the placenames and distance apart
parse :: String -> (String, String, Int)
parse str = (place1, place2, read distance :: Int)
  where (_,_,_, [place1, place2, distance]) = (str =~ "(.+) +to +(.+) += +(\\d+)") :: (String,String,String, [String])

-- Given a list, returns all the pairs as you travel along the list
-- [1,2,3,4] -> [(1, 2), (2,3), (3,4)]
getPairs list = zip list $ tail list
