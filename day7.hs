module Day7 where
import Common
import Prelude hiding (lookup)
import Text.Regex.Posix hiding (empty)
import Data.Map hiding (foldl, map)
import Data.Bits
import Data.Word
import Text.Read
import Data.Text (strip)

part1 content = container (empty :: Map String Word16) (map buildExpression $ lines content) 0

container map exps depth = let (resultExp, resultMap) = loop map exps
                           in if depth < 150 then container resultMap resultExp (depth+1) else resultMap

loop map exps = foldl foldLambda ([] :: [Expression], map) exps

foldLambda :: ([Expression], Map String Word16) -> Expression -> ([Expression], Map String Word16)
foldLambda (exps, dict) exp = let (result, newDict) = operate exp dict
                              in (if result then exps else exp:exps, newDict)

data Opcode = AND | OR | LSHIFT | RSHIFT | NOT | VALUE
              deriving (Show)

data Expression = BINARY Opcode String String String
                | UNARY Opcode String String
                deriving (Show)

buildExpression str = let (_,_,_, [p1s, instruction, p2, dest]) = parse str
                          p1 = init p1s
  in case instruction of "AND" -> BINARY AND dest p1 p2
                         "OR" -> BINARY OR dest p1 p2
                         "RSHIFT" -> BINARY RSHIFT dest p1 p2
                         "LSHIFT" -> BINARY LSHIFT dest p1 p2
                         "NOT" -> UNARY NOT dest p2
                         "" -> UNARY VALUE dest p2

parse :: String -> (String,String,String, [String])
parse str = str =~ "([a-z0-9]* )*(NOT|AND|OR|RSHIFT|LSHIFT)* *([a-z0-9]+) -> ([a-z]+)"

executeBinaryExpression :: Opcode -> Word16 -> Word16 -> Word16
executeBinaryExpression AND v1 v2 = (.&.) v1 v2
executeBinaryExpression OR v1 v2 = (.|.) v1 v2
executeBinaryExpression LSHIFT v1 v2 = shiftL v1 (fromIntegral v2 :: Int)
executeBinaryExpression RSHIFT v1 v2 = shiftR v1 (fromIntegral v2 :: Int)

executeUnaryExpression :: Opcode -> Word16 -> Word16
executeUnaryExpression NOT v1 = complement v1
executeUnaryExpression VALUE v1 = v1

resolve dict val = case readMaybe val of Just number -> Just number
                                         Nothing -> lookup val dict

operate exp@(BINARY op dest p1 p2) dict = let
  maybe1 = resolve dict p1
  maybe2 = resolve dict p2
  in case maybe1 of Nothing -> (False, dict)
                    Just v1 -> case maybe2 of Nothing -> (False, dict)
                                              Just v2 -> (True, insert dest (executeBinaryExpression op v1 v2) dict)

operate exp@(UNARY op dest p1) dict = let
  maybe1 = resolve dict p1
  in case maybe1 of Nothing -> (False, dict)
                    Just v1 -> (True, insert dest (executeUnaryExpression op v1) dict)
