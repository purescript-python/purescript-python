module Main where
import System.Exit
import System.Environment
import System.Directory (createDirectoryIfMissing, getCurrentDirectory)
import System.FilePath ((</>), joinPath, takeFileName)

import Text.Printf (printf)

import Data.Aeson hiding (Options)
import Data.Aeson.Types hiding (Options)

import qualified Data.List as List

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as T

import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.Encoding as L
import qualified Data.ByteString as B
import qualified Data.Set as S


import qualified Language.PureScript.AST.SourcePos as SP
import Language.PureScript.CoreImp.AST ( AST
                                       , withSourceSpan
                                       , getSourceSpan
                                       , everywhere
                                       )

import Language.PureScript.CoreFn
import Language.PureScript.CoreFn.FromJSON
import Language.PureScript.Errors (MultipleErrors)
import Language.PureScript.Options (Options(..))
import qualified Language.PureScript as P
import Language.PureScript.Names ( moduleNameFromString
                                 , isBuiltinModuleName )


import Language.PureScript.CodeGen.Py (moduleToJS)
import Language.PureScript.CodeGen.Py.Printer (Py, bindSExpr)
import Language.PureScript.CodeGen.Py.Eval (finally)
import Language.PureScript.CodeGen.Py.Common (escape)

import Control.Monad.Supply

import Data.Text.Prettyprint.Doc.Render.Text (renderStrict)
import Data.Text.Prettyprint.Doc (Doc, layoutPretty, defaultLayoutOptions)

import Control.Monad.Reader (MonadReader(..))
import qualified Control.Monad.State as State

import Monads.STEither

instance MonadReader Options (STEither Options MultipleErrors) where
    ask = STEither State.get
    local r (STEither m) = STEither (State.modify r >> m)


defaultOpts :: Options
defaultOpts =
    Options { optionsVerboseErrors = True
            , optionsNoComments = False
            , optionsCodegenTargets = S.empty
            }

main :: IO ()
main = do
    opts <- getArgs
    case opts of
        [ "--out-python"
         , baseOutDir
         , "--corefn-entry"
         , moduleParts
         , "--out-ffi-dep"
         , ffiDepPath] -> do
            ffiDeps <-
              fixPointCG
                baseOutDir
                S.empty
                (S.empty, [moduleNameFromString $ T.pack moduleParts])
            T.writeFile ffiDepPath (T.unlines $ map T.pack ffiDeps)
        _ ->  putStrLn "Malformed options, expect form --out-python <dir0> --corefn-entry <A.B.C> --out-ffi-dep <xxx>." >> exitFailure


fixPointCG :: FilePath -> S.Set FilePath -> (S.Set P.ModuleName, [P.ModuleName]) -> IO [FilePath]
fixPointCG baseOutDir ffiPathReferred (importedModules, moduleImportDeque) =
  case moduleImportDeque of
    [] -> return $ S.toList ffiPathReferred
    m:ms
     | m `S.member` importedModules || isBuiltinModuleName m ->
       fixPointCG baseOutDir ffiPathReferred (importedModules, ms)
     | otherwise -> do
       (newModsToImport, newFFIReferred) <- cg baseOutDir m
       fixPointCG
        baseOutDir
        (S.union ffiPathReferred newFFIReferred)
        (S.insert m importedModules, newModsToImport ++ ms)

-- code generation for each  module
cg :: FilePath -> P.ModuleName -> IO ([P.ModuleName], S.Set FilePath)
cg baseOutDir coreFnMN = do
  pwd      <- getCurrentDirectory
  -- TODO: customizable `output` directory
  let jsonFile = joinPath
        [ pwd
        , "output"
        , runModuleName [] [] coreFnMN
        , "corefn.json"
        ]

  jsonText <- T.decodeUtf8 <$> B.readFile jsonFile
  let module' = jsonToModule $ parseJson jsonText
      -- getting the module name
      mn      = moduleName module'
      -- getting the module path
      mp      = modulePath module'
      -- name of the generated python package
      package = takeFileName baseOutDir
  hasForeign <- case flip State.runStateT defaultOpts . runSTEither .runSupplyT 5 $
        moduleToJS module' (T.pack package) of
      Left e -> print (e :: MultipleErrors) >> exitFailure
      Right (((hasForeign, ast), _), _) -> do
        let implCode =
                legalizedCodeGen
                  (runModuleName [package] ["pure"] mn)
                  (joinPath [pwd, mp])
                  (finally $ everywhere (astSSToAbsPath pwd) ast)

            outDir = runToModulePath [pwd, baseOutDir] [] mn
            implSrcPath = outDir </> "pure.src.py"
            implLoaderPath = outDir </> "pure.py"

        createDirectoryIfMissing True outDir
        T.writeFile implSrcPath implCode
        T.writeFile implLoaderPath loaderCode
        return hasForeign
  
  let newModsToImport = map snd (moduleImports module')
  let newFModToAdd = S.fromList [mp | hasForeign]
  return (newModsToImport, newFModToAdd)

runToModulePath ::  [FilePath] -> [FilePath] -> P.ModuleName -> String
runToModulePath prefix suffix (P.ModuleName pns) =
        joinPath .
        (prefix++) . (++suffix) .
        map T.unpack $ (P.runProperName <$> pns)

runModuleName ::  [FilePath] -> [FilePath] -> P.ModuleName -> String
runModuleName prefix suffix (P.ModuleName pns) =
        List.intercalate "." .
        (prefix++) . (++suffix) .
        map T.unpack $ (P.runProperName <$> pns)

parseJson :: Text -> Value
parseJson text
  | Just fileJson <- decode . L.encodeUtf8 $ L.fromStrict text = fileJson
  | otherwise = error "Bad json"

jsonToModule :: Value -> Module Ann
jsonToModule value =
  case parse moduleFromJSON value of
    Success (_, r) -> r
    _ -> error "Bad corefn"

astSSToAbsPath :: FilePath -> AST -> AST
astSSToAbsPath pwd n =
  case getSourceSpan n of
    Nothing -> n
    Just ss ->
      withSourceSpan (ss {P.spanName = joinPath [pwd, SP.spanName ss]}) n

doc2Text :: Doc ann -> T.Text
doc2Text = renderStrict . layoutPretty defaultLayoutOptions

legalizedCodeGen :: String -> FilePath -> Doc Py -> Text
legalizedCodeGen mName mPath sexpr =
    T.unlines
      [
        T.pack "from py_sexpr.terms import *"
      , T.pack "from py_sexpr.stack_vm.emit import module_code"
      , doc2Text (bindSExpr "res" sexpr)
      , T.pack $
          printf
            "res = module_code(res, filename=%s, name=%s)"
            (escape mPath)
            (escape mName)
      ]

loaderCode :: Text
loaderCode =
    T.unlines . map T.pack $
      [
        "from purescripto import LoadPureScript"
      , "__py__ = globals()"
      , "__ps__ = LoadPureScript(__file__, __name__)"
      , "__all__ = list(__ps__)"
      , "__py__.update(__ps__)"
      ]
