-- | pretty print top down representations
module Topdown.Pretty(PrettyTopdown) where
import Topdown.Core
import Data.Text.Prettyprint.Doc
import StringEscape (escape)

data PrettyTopdown

instance Topdown (Doc PrettyTopdown) where
  tfFloat = pretty
  tfInt = pretty
  tfStr = pretty . escape
  tfBool = pretty
  tfUnit = pretty "None"
  tfCons n xs = pretty n <> vsep [align . tupled $ xs]
  tfSeq = list
  tfVar = pretty
  tfAcc subject attr = subject <> pretty "." <> pretty attr
