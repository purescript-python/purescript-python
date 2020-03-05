-- | pretty print top down representations
{-# LANGUAGE OverloadedStrings #-}
module Topdown.Raw where
import Topdown.Core
import Data.Text.Prettyprint.Doc
import StringEscape (escape)
import Data.ByteString.Conversion (toByteString)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.UTF8 as BLU


instance Topdown ByteString where
  tfFloat = toByteString
  tfInt = toByteString
  tfStr = toByteString . escape
  tfBool True = "True"
  tfBool False = "False"
  tfUnit = "None"
  tfCons n xs = BLU.fromString n <> "(" <> B.intercalate "," xs <> ")"
  tfSeq xs = "[" <> B.intercalate "," xs <> "]"
  tfVar = BLU.fromString
  tfAcc subject attr = subject <> "." <> BLU.fromString attr
