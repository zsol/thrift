{-# LANGUAGE OverloadedStrings #-}
module Thrift.Protocol.LaxBinary
       ( module Thrift.Protocol
       , LaxBinaryProtocol(..)
       ) where

import Control.Monad (liftM)
import Data.Text.Lazy.Encoding (decodeUtf8)

import Thrift.Protocol
import Thrift.Protocol.Binary
import Thrift.Protocol.Internal
import Thrift.Transport


data LaxBinaryProtocol a = Transport a => LaxBinaryProtocol a

instance Protocol LaxBinaryProtocol where
  writeMessageBegin p (n, t, s) = do
    writeString p n
    writeByte p (fromIntegral $ fromEnum t)
    writeI32 p s

  readMessageBegin p = do
    sz <- readI32 p
    if (sz < 0)
      then verifyAndReadMessageBegin p sz
      else do
        name <- decodeUtf8 `liftM` tReadAll (getTransport p) (fromIntegral sz)
        typ <- readByte p
        seqid <- readI32 p
        return (name, toEnum $ fromIntegral typ, seqid)


  getTransport (LaxBinaryProtocol t) = getTransport $ BinaryProtocol t

  writeMessageEnd   (LaxBinaryProtocol t) = writeMessageEnd   $ BinaryProtocol t

  writeStructBegin (LaxBinaryProtocol t) = writeStructBegin $ BinaryProtocol t
  writeStructEnd   (LaxBinaryProtocol t) = writeStructEnd   $ BinaryProtocol t
  writeFieldBegin  (LaxBinaryProtocol t) = writeFieldBegin  $ BinaryProtocol t
  writeFieldEnd    (LaxBinaryProtocol t) = writeFieldEnd    $ BinaryProtocol t
  writeFieldStop   (LaxBinaryProtocol t) = writeFieldStop   $ BinaryProtocol t
  writeMapBegin    (LaxBinaryProtocol t) = writeMapBegin    $ BinaryProtocol t
  writeMapEnd      (LaxBinaryProtocol t) = writeMapEnd      $ BinaryProtocol t
  writeListBegin   (LaxBinaryProtocol t) = writeListBegin   $ BinaryProtocol t
  writeListEnd     (LaxBinaryProtocol t) = writeListEnd     $ BinaryProtocol t
  writeSetBegin    (LaxBinaryProtocol t) = writeSetBegin    $ BinaryProtocol t
  writeSetEnd      (LaxBinaryProtocol t) = writeSetEnd      $ BinaryProtocol t

  writeBool   (LaxBinaryProtocol t) = writeBool   $ BinaryProtocol t
  writeByte   (LaxBinaryProtocol t) = writeByte   $ BinaryProtocol t
  writeI16    (LaxBinaryProtocol t) = writeI16    $ BinaryProtocol t
  writeI32    (LaxBinaryProtocol t) = writeI32    $ BinaryProtocol t
  writeI64    (LaxBinaryProtocol t) = writeI64    $ BinaryProtocol t
  writeDouble (LaxBinaryProtocol t) = writeDouble $ BinaryProtocol t
  writeString (LaxBinaryProtocol t) = writeString $ BinaryProtocol t
  writeBinary (LaxBinaryProtocol t) = writeBinary $ BinaryProtocol t


  readMessageEnd   (LaxBinaryProtocol t) = readMessageEnd   $ BinaryProtocol t

  readStructBegin (LaxBinaryProtocol t) = readStructBegin $ BinaryProtocol t
  readStructEnd   (LaxBinaryProtocol t) = readStructEnd   $ BinaryProtocol t
  readFieldBegin  (LaxBinaryProtocol t) = readFieldBegin  $ BinaryProtocol t
  readFieldEnd    (LaxBinaryProtocol t) = readFieldEnd    $ BinaryProtocol t
  readMapBegin    (LaxBinaryProtocol t) = readMapBegin    $ BinaryProtocol t
  readMapEnd      (LaxBinaryProtocol t) = readMapEnd      $ BinaryProtocol t
  readListBegin   (LaxBinaryProtocol t) = readListBegin   $ BinaryProtocol t
  readListEnd     (LaxBinaryProtocol t) = readListEnd     $ BinaryProtocol t
  readSetBegin    (LaxBinaryProtocol t) = readSetBegin    $ BinaryProtocol t
  readSetEnd      (LaxBinaryProtocol t) = readSetEnd      $ BinaryProtocol t

  readBool   (LaxBinaryProtocol t) = readBool   $ BinaryProtocol t
  readByte   (LaxBinaryProtocol t) = readByte   $ BinaryProtocol t
  readI16    (LaxBinaryProtocol t) = readI16    $ BinaryProtocol t
  readI32    (LaxBinaryProtocol t) = readI32    $ BinaryProtocol t
  readI64    (LaxBinaryProtocol t) = readI64    $ BinaryProtocol t
  readDouble (LaxBinaryProtocol t) = readDouble $ BinaryProtocol t
  readString (LaxBinaryProtocol t) = readString $ BinaryProtocol t
  readBinary (LaxBinaryProtocol t) = readBinary $ BinaryProtocol t
