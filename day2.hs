{-# LANGUAGE TemplateHaskell #-}
module Day2 where
import Common
import Test.QuickCheck
import Data.List.Split
import Data.List

part1 = sumLineOutput surfaceArea
part2 = sumLineOutput (\ sides -> product sides + 2 * sum (twoShortest sides))

sidesToList :: String -> [Int]
sidesToList = map read . splitOn "x"

sumLineOutput x content = sum.map (x.sidesToList) $ lines content

twoShortest sides = take 2 (sort sides)
surfaceArea sides@[l,w,h] = (2*l*w + 2*w*h + 2*h*l) + product (twoShortest sides)

-- Unit Tests --
