-- | This module implements a data file format,
-- tentatively called `topdown`.
module Topdown.Core(Topdown(..)) where

class Topdown a where
  tfFloat :: Double -> a
  tfInt :: Integer -> a
  tfStr :: String -> a
  tfBool :: Bool -> a
  tfUnit :: a
  tfCons :: String -> [a] -> a
  tfSeq :: [a] -> a
  tfVar :: String -> a
  tfAcc :: a -> String -> a
