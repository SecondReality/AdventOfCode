module Day12 where
import Common
import Data.Aeson
import Data.ByteString.Lazy.Char8 (pack)
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as M
import qualified Data.Text as D

part1 content = maybe 0 sumStructure (parseJson content)
part2 content = maybe 0 sumNotRedStructure (parseJson content)

parseJson :: String -> Maybe Value
parseJson s= decode $ pack s

sumStructure :: Value -> Integer
sumStructure (Number x) = floor $ toRational x
sumStructure (Array x) = V.sum $ V.map sumStructure x
sumStructure (Object x) = sum $ map sumStructure $ M.elems x
sumStructure _ = 0

sumNotRedStructure :: Value -> Integer
sumNotRedStructure (Number x) = floor $ toRational x
sumNotRedStructure (Array x) = V.sum $ V.map sumNotRedStructure x
sumNotRedStructure (Object x) = if isRed then 0 else sumMapElements
  where isRed = elem (String $ D.pack "red") $ M.elems x
        sumMapElements = sum $ map sumNotRedStructure $ M.elems x
sumNotRedStructure _ = 0