module Thrift.Protocol.Internal
       ( version_mask
       , version_1
       , verifyAndReadMessageBegin
       ) where

import Control.Exception (throw)
import Data.Bits
import Data.Int
import Data.Text.Lazy (Text)

import Thrift.Transport
import Thrift.Protocol


version_mask :: Int32
version_mask = 0xffff0000

version_1 :: Int32
version_1    = 0x80010000

verifyAndReadMessageBegin :: (Transport t, Protocol a)
                             => a t -> Int32 -> IO (Text, MessageType, Int32)
verifyAndReadMessageBegin p ver = do
  if (ver .&. version_mask /= version_1)
    then throw $ ProtocolExn PE_BAD_VERSION "Missing version identifier"
    else do
         s <- readString p
         sz <- readI32 p
         return (s, toEnum $ fromIntegral $ ver .&. 0xFF, sz)
