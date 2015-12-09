-- regex-pcre-builtin
module Day6 where
import Common
import Linear.V2
import Prelude hiding (replicate, head, last, zip, sum)
import Data.Vector hiding (sequence, map, reverse)
import Text.Regex.PCRE

part1 content = count True $ toList (applyCommandList binaryLight content False)
part2 content = sum (applyCommandList intLight content 0)

binaryLight "turn off" = const False
binaryLight "turn on" = const True
binaryLight "toggle" = not

intLight "turn off" x = if x > 0 then x-1 else x
intLight "turn on" x = x+1
intLight "toggle" x = x+2

applyCommandList lightType content initialValue = foldr' ((\ ( fn, p1, p2) grid -> updateGrid (lightType fn) p1 p2 grid).parseLine) (emptyGrid initialValue) (fromList $ reverse (lines content))

emptyGrid = createLightGrid 1000 1000

createLightGrid width height = replicate (height*width)

readVec str1 str2 = V2 (read str1) (read str2)

parseLine str = let
  (_,_,_, [command, x1, y1, x2, y2]) = groups str
  in (command, readVec x1 y1, readVec x2 y2)

groups :: String -> (String,String,String, [String])
groups str = str =~ "(.*) +(\\d+),(\\d+) +through +(\\d+),(\\d+)"

updateGrid fn p1 p2 lightGrid = let
  indexes = fromList $ indices1d p1 p2
  in unsafeAccumulate (\ a b -> fn a) lightGrid ( zip indexes (replicate (Data.Vector.length indexes) 2 ) )

indices (V2 x1 y1) (V2 x2 y2) = sequence [[x1..x2], [y1..y2]]
indices1d p1 p2 = map (\ [x,y]-> index1d x y) $ indices p1 p2
index1d x y = y * 1000 + x
