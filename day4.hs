-- Requires cryptohash and base16-bytestring for MD5 and encoding
module Day4 where
import Common
import Data.ByteString.Char8 (unpack, pack)
import Data.ByteString.Base16
import Crypto.Hash.MD5 as MD5
import Data.List

part1 content = findLowestMd5 content "00000"
part2 content = findLowestMd5 content "000000"

findLowestMd5 prefix beginsWith = Data.List.find (\ x -> isPrefixOf beginsWith (md5String (prefix ++ show x))) [1..]

md5String = unpack . Data.ByteString.Base16.encode . MD5.hash . pack
