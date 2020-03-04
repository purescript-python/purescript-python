
{-# LANGUAGE
      OverloadedStrings
#-}
module Language.PureScript.CodeGen.Py.Naming where
import Language.PureScript.Crash
import Language.PureScript.Names
import Data.Text (Text)

identToPy :: Ident -> Text
identToPy (Ident name) = name
identToPy (GenIdent _ _) = internalError "GenIdent in identToPy"
identToPy UnusedIdent = "$__unused"
