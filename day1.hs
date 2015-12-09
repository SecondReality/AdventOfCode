{-# LANGUAGE TemplateHaskell #-}
module Day1 where
import Common
import Test.QuickCheck
import Data.List

part1 = sum.toNumbers
part2 content = elemIndex (-1) $ travel (toNumbers content)

toNumbers = map (\x -> if x=='(' then 1 else -1 )

travel = scanl (+) 0

-- Unit Tests --

newtype RandomString = RandomString String deriving (Eq,Show)
instance Arbitrary RandomString where
    arbitrary = sized $ \s -> do
      n <- choose (0, s `min` 50)
      xs <- vectorOf n (choose (')', '('))
      return (RandomString xs)

prop_part1 :: RandomString -> Bool
prop_part1 (RandomString str) = count '(' str - count ')' str == part1 str

return []
runTests = $verboseCheckAll --quickCheckAll
