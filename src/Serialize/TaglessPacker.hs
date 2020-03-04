module Serialize.TaglessPacker(TaglessDump(..), runPacker, Serial, Lines) where

import Control.Monad.State
import qualified Data.Map as Map
import qualified Data.Text as Text
import Text.Printf (printf)
import Data.List (intercalate)

type Text = Text.Text

class TaglessDump a where
  tfFloat :: Double -> a
  tfInt :: Integer -> a
  tfStr :: String -> a
  tfBool :: Bool -> a
  tfUnit :: a
  tfCons :: String -> [a] -> a
  tfSeq :: [a] -> a
  tfVar :: String -> a

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

type Lines = [Text]

instance TaglessDump (Serial Lines)  where
  tfFloat = return . pure . Text.pack . ('f':) . show
  tfInt = return . pure . Text.pack . ('i':) . show
  tfBool = return . pure . Text.pack . ('b':) . show
  tfUnit = return . pure . Text.pack $ "n"
  tfStr s = do
    si <- stringCompress s
    return [Text.pack ('s':show si)]
  tfVar s = do
    si <- stringCompress s
    return [Text.pack ('v':show si)]
  tfCons constructor args = do
    args <- concat <$> sequence args
    constructor <- stringCompress constructor
    let n = show (length args)
        loads :: Lines
        loads = Text.pack ('c':n ++ " " ++ show constructor) : args
    return loads
  tfSeq args = do
    args <- concat <$> sequence args
    let n = show (length args)
        loads :: Lines
        loads = Text.pack ('l':n):args
    return loads

runPacker :: Serial Lines -> Text
runPacker m =
  let (lines, compressed) = runState m Map.empty
      compressedSize = Map.size compressed
      genCompress :: Lines -> (String, Int) -> Lines
      genCompress init (string, intIdx) =
        let head = show intIdx ++ " " ++ show (length string)
        in Text.pack (head ++ "\n" ++ string):init
      compressTable = foldl genCompress [] (Map.toList compressed)
  in Text.intercalate (Text.pack "\n") $ compressTable ++ lines
