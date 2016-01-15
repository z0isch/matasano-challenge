{-# LANGUAGE OverloadedStrings #-}

module Utils where

import           Data.Bits
import           Data.ByteString        (ByteString)
import qualified Data.ByteString        as B
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8  as BC
import           Data.Char
import           Data.Either
import           Data.List
import           Data.Map.Strict        (Map)
import qualified Data.Map.Strict        as Map
import           Data.Word

data Base16String = Base16String ByteString
  deriving (Show)
data Base64String = Base64String ByteString
  deriving (Show)

decodeBase64String :: Base64String -> ByteString
decodeBase64String (Base64String s) = head $ rights [B64.decode s]

decodeBase16String :: Base16String -> ByteString
decodeBase16String (Base16String s) = fst $ B16.decode s

fixedXOR :: ByteString -> ByteString -> ByteString
fixedXOR key = B.pack . B.zipWith xor key

base16ToBase64 :: Base16String -> Base64String
base16ToBase64 (Base16String bs) = Base64String $ B64.encode $ fst $ B16.decode bs

repeatingKeyXOR :: ByteString -> ByteString -> ByteString
repeatingKeyXOR key = B.pack . zipWith xor (cycle $ B.unpack key) . B.unpack

editDistance :: ByteString -> ByteString -> Int
editDistance b1 b2 = foldl (\d (w1,w2) -> d + popCount (w1 `xor` w2)) 0 $ B.zip b1 b2

xorWithOneChar :: ByteString -> [((Char,String),Double)]
xorWithOneChar hs = sortBy (\(_,x) (_,y) -> compare y x) $
  map ((\(x,c) -> ((x,c), weight c)) . (\c -> (head (BC.unpack c),BC.unpack (c `fixedXOR` hs))) . BC.pack . replicate (B.length hs)) [(chr 32)..(chr 126)]--['a'..'z']++['A'..'Z']++['1'..'9']

weight :: String -> Double
weight s = Map.foldlWithKey g 0 ( histogram s) / totLength
  where
    totLength = genericLength s
    histogram xs = Map.fromList [ (head l, length l) | l <- group (sort xs) ]
    g w c s = case getFrequency c of (Just f) -> w + (fromIntegral s * f)
                                     Nothing -> if isNumber c || isPunctuation c || isSeparator c || isSpace c then w else w - (26 * totLength)
    getFrequency c = Map.lookup (toUpper c) frequencyMap

frequencyMap :: Map Char Double
frequencyMap = Map.fromList [('A',8.34),('B',1.54),('C',2.73),('D',4.14),('E',12.60),('F',2.03),('G',1.92),('H',6.11),('I',6.71),('J',0.23),('K',0.87),('L',4.24),('M',2.53),('N',6.80),('O',7.70),('P',1.66),('Q',0.09),('R',5.68),('S',6.11),('T',9.37),('U',2.85),('V',1.06),('W',2.34),('X',0.20),('Y',2.04),('Z',0.06)]

bsSplit :: Int -> ByteString -> [ByteString]
bsSplit i b = go i b []
  where
    go i b bs
      | B.null b = bs
      | otherwise = go i (B.drop i b) (bs ++ [B.take i b])
