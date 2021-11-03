{-# LANGUAGE OverloadedStrings
           , GADTs
#-}
module Language.PureScript.CodeGen.Diana.Common where

import Data.String
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Prettyprint.Doc (Doc, pretty)
-- import qualified Data.Char as C
-- import Text.Printf (printf)


class SpecialName a where
  polyDiv :: a
  newObject :: a
  thisName :: a
  importName :: a
  updateRecord :: a
  ranger :: a

instance SpecialName Text where
  polyDiv = "div"
  newObject = "new"
  thisName = "this"
  importName = "require"
  updateRecord = "Dict.update"
  ranger = "Enum.range"

prettyText :: Text -> Doc a
prettyText = pretty

instance SpecialName (Doc a) where
  polyDiv = prettyText polyDiv
  newObject = prettyText newObject
  thisName = prettyText thisName
  importName = prettyText importName
  updateRecord = prettyText updateRecord
  ranger = prettyText ranger


unmanglePrefix :: Text
unmanglePrefix = "special😅"

-- | unmangle or specialize names
--   current occurrences:
--   - __all__
--   - import
--   - this

unmangle :: Text -> Text
unmangle = T.append unmanglePrefix

data SourceLoc
    = SourceLoc
      { line     :: Int
      , col      :: Int
      , filename :: String
      }

data BoxedName where
    This     :: BoxedName
    Import   :: BoxedName
    Normal :: Text -> BoxedName


instance IsString BoxedName where
    fromString = mkName . T.pack

mkName :: Text -> BoxedName
mkName text
    | [_, a] <- T.splitOn unmanglePrefix text =
        case a of
            -- these are RHS only
            "this" -> This
            "import" -> Import
            _ -> Normal $ T.replace "$" "☆" a
    | otherwise = Normal $ 
        if T.isInfixOf "$" text then T.replace "$" "☆" text
        else "x_" <> text

mustNorm :: BoxedName -> Text
mustNorm (Normal a) = a
mustNorm _ = error "invalid name"

forceNorm :: BoxedName -> Text
forceNorm (Normal a) = a
forceNorm This = thisName
forceNorm Import = importName


{-# COMPLETE MustNorm #-}
pattern MustNorm a <- (mustNorm -> a)