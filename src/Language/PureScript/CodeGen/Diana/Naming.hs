
{-# LANGUAGE
      OverloadedStrings
#-}
module Language.PureScript.CodeGen.Diana.Naming where
import Language.PureScript.Crash
import Language.PureScript.Names
import Data.Text (Text, replace)

identToDiana :: Ident -> Text
identToDiana (Ident name) = name
identToDiana (GenIdent _ _) = internalError "GenIdent in identToDiana"
identToDiana UnusedIdent = "__unused"


properToDiana = runProperName

moduleNameToDiana (ModuleName mn) = replace "." "_" mn