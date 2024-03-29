{-# LANGUAGE OverloadedStrings
           , GADTs
#-}
module Language.PureScript.CodeGen.Py.Common where

import Data.String
import Data.Text (Text)
import qualified Data.Text as T
-- import qualified Data.Char as C
-- import Text.Printf (printf)

unmanglePrefix :: Text
unmanglePrefix = "special@"

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
    UnMangled :: Text -> BoxedName
    Mangled   :: Text -> BoxedName


instance IsString BoxedName where
    fromString = mkName . T.pack

mkName :: Text -> BoxedName
mkName text
    | [_, a] <- T.splitOn unmanglePrefix text =
        case a of
            -- these are RHS only
            "this" -> This
            "import" -> Import
            _ -> UnMangled a
    | otherwise = Mangled text

unbox :: BoxedName -> Maybe String
unbox = \case
    UnMangled n -> Just $ T.unpack n
    Mangled n   -> Just $ "ps_" ++ T.unpack n
    _ -> Nothing


pattern Unbox :: String -> BoxedName
pattern Unbox a <- (unbox -> Just a)
