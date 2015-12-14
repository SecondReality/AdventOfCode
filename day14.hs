module Day14 where
import Common
import Text.Regex.PCRE
import Data.List

part1 content = maximum $ map (distanceTravelled 2503) (parseReindeers content)
part2 content = maximum $ map sum $ transpose $ map (scoreAtTime content) [1..2503]

distanceTravelled time (name, speed, sprintDuration, restDuration) = speed * (sprintDuration * div time period + min remainder sprintDuration)
  where period = sprintDuration + restDuration
        remainder = mod time period

type Reindeer = (String, Int, Int, Int)

parseReindeers :: String -> [Reindeer]
parseReindeers content = map parseReindeer $ lines content

parseReindeer :: String -> Reindeer
parseReindeer str = (name, read speed, read sprintDuration, read restDuration)
  where (_,_,_, [name, speed, sprintDuration, restDuration]) = (str =~ "(\\w+) can fly (\\d+) km\\/s for (\\d+) seconds, but then must rest for (\\d+) seconds.") :: (String,String,String, [String])

-- Part 2
distancesToScore xs = map (\ x -> if x == maximum xs then 1 else 0) xs
scoreAtTime content time = distancesToScore $ map (distanceTravelled time) (parseReindeers content)