{-# LANGUAGE OverloadedStrings #-}
module Topdown.Topdown(serialize, Serial) where
import Topdown.Core
import Control.Monad.State

import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Conversion (toByteString)
import qualified Data.Map as Map
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.UTF8 as BU

type Serial a = State (Map.Map String Int) a

stringCompress :: String -> Serial Int
stringCompress s = do
  direct <- get
  case Map.lookup s direct of
    Nothing -> do
      let i = Map.size direct
      put (Map.insert s i direct)
      return i
    Just i -> return i

instance Topdown (Serial ByteString)  where
  tfFloat = return . ("f"<>) . toByteString
  tfInt = return . ("i"<>) . toByteString
  tfBool True = return "bt"
  tfBool False = return "bf"
  tfUnit = return "n"
  tfAcc subj attr = do
      s <- subj
      return $ "a" <> BU.fromString attr <> "\n" <> s
  tfStr s = do
    si <- stringCompress s
    return $ "s" <> toByteString si
  tfVar s = do
    si <- stringCompress s
    return $ "v" <> toByteString si
  tfCons constructor args = do
    args <- sequence args
    constructor <- toByteString <$> stringCompress constructor
    let n = toByteString (length args)
        loads = "c" <> n <> " " <>  constructor <> "\n" <> B.intercalate "\n" args
    return loads
  tfSeq args = do
    args <- sequence args
    let n = toByteString (length args)
        loads = "l" <> n <> "\n" <> B.intercalate "\n" args
    return loads

serialize :: Serial ByteString -> ByteString
serialize m =
  let (lines, compressed) = runState m Map.empty
      compressedSize = Map.size compressed
      genCompress :: ByteString -> (String, Int) -> ByteString
      genCompress init (string, intIdx) =
        toByteString intIdx <> " " <> toByteString (length string)
            <> "\n" <> BU.fromString string
            <> "\n" <> init
      compressTable = foldl genCompress "" (Map.toList compressed)
  in toByteString compressedSize <> "\n" <> compressTable <> "\n" <> lines

