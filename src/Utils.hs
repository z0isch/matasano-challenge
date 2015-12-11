{-# LANGUAGE OverloadedStrings #-}

module Utils where

import           Data.Bits
import           Data.ByteString        (ByteString)
import qualified Data.ByteString        as B
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Base64 as B64

data Base16String = Base16String ByteString
  deriving (Show)
data Base64String = Base64String ByteString
  deriving (Show)

decodeBase16String :: Base16String -> ByteString
decodeBase16String (Base16String s) = fst $ B16.decode s

fixedXOR :: ByteString -> ByteString -> ByteString
fixedXOR key = B.pack . B.zipWith xor key

base16ToBase64 :: Base16String -> Base64String
base16ToBase64 (Base16String bs) = Base64String $ B64.encode $ fst $ B16.decode bs

repeatingKeyXOR :: ByteString -> ByteString -> ByteString
repeatingKeyXOR key = B.pack . zipWith xor (cycle $ B.unpack key) . B.unpack
