module StringEscape(escape) where

import qualified Data.Char as C

escapeImpl :: String -> String -> String
escapeImpl [] r = r
escapeImpl (c:cs) r = case c of
    '"'  -> "\\\"" ++ tl
    '\\' -> "\\\\" ++ tl
    _ | C.isPrint c && not (C.isControl c) -> c:tl
      | otherwise -> C.showLitChar c tl
    where
        tl    = escapeImpl cs r

escape :: String -> String
escape s = '\"' : escapeImpl s "\""
