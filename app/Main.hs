module Main where
import System.Exit
import System.Environment
import System.Directory (createDirectoryIfMissing, getCurrentDirectory)
import System.FilePath ((</>), joinPath, takeFileName)

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
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.UTF8 as BSU
import qualified Data.ByteString.Lazy.UTF8 as BLU
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
import Language.PureScript.CodeGen.Py.Serializer ()
import Language.PureScript.CodeGen.Py.Eval (finally, EvalJS)
import Control.Monad.Supply

import Data.Text.Prettyprint.Doc.Render.Text (renderStrict)
import Data.Text.Prettyprint.Doc (Doc, layoutPretty, defaultLayoutOptions)

import Control.Monad (when)
import Control.Monad.Reader (MonadReader(..))
import qualified Control.Monad.State as State

import Monads.STEither
import Topdown.Pretty (PrettyTopdown)
import Topdown.Raw ()
import Topdown.Topdown (serialize)
import Codec.Archive.Zip
import StringEscape (escape)

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
        [ "--py-dir"
         , baseOutDir
         , "--entry-mod"
         , moduleParts
         , "--ffi-dep"
         , ffiDepPath
         , "--out-format"
         , outFormat] -> do
            ffiDeps <-
              fixPointCG
                (read outFormat)
                baseOutDir
                S.empty
                (S.empty, [moduleNameFromString $ T.pack moduleParts])
            T.writeFile ffiDepPath (T.unlines $ map T.pack ffiDeps)
        _ ->
          putStrLn
            "Malformed options, expect form --py-dir <dir0> --entry-mod <A.B.C> --ffi-dep <xxx> --out-format [Pretty|Compact|Compressed]." >> exitFailure

data OutFormat = Pretty | Compact | Compressed deriving (Read)

-- code generation for used modules
fixPointCG :: OutFormat -> FilePath -> S.Set FilePath -> (S.Set P.ModuleName, [P.ModuleName]) -> IO [FilePath]
fixPointCG outFormat baseOutDir ffiPathReferred (importedModules, moduleImportDeque) =
  case moduleImportDeque of
    [] -> return $ S.toList ffiPathReferred
    m:ms
     | m `S.member` importedModules || isBuiltinModuleName m ->
       fixPointCG outFormat baseOutDir ffiPathReferred (importedModules, ms)
     | otherwise -> do
       (newModsToImport, newFFIReferred) <- cg outFormat baseOutDir m
       fixPointCG
        outFormat
        baseOutDir
        (S.union ffiPathReferred newFFIReferred)
        (S.insert m importedModules, newModsToImport ++ ms)

toStrict :: BL.ByteString -> B.ByteString
toStrict = B.concat . BL.toChunks

-- code generation for each  module
cg :: OutFormat -> FilePath -> P.ModuleName -> IO ([P.ModuleName], S.Set FilePath)
cg outFormat baseOutDir coreFnMN = do
  pwd      <- getCurrentDirectory
  let qualifiedMN = runModuleName [] [] coreFnMN
  -- TODO: customizable `output` directory
  let jsonFile = joinPath
        [ pwd
        , "output"
        , qualifiedMN
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
        let augmentedAST = everywhere (astSSToAbsPath pwd) ast
            outDir = runToModulePath [pwd, baseOutDir] [] mn
            to :: FilePath -> FilePath
            to = (outDir </>)
            implCode :: forall a. EvalJS a => a
            implCode = finally augmentedAST

        putStrLn $ "Codegen Python for " ++ qualifiedMN
        createDirectoryIfMissing True outDir

        case outFormat of
            Compact ->
                BL.writeFile (to "pure.raw.py") $
                    BLU.fromString ("(" ++ escape mp ++ ",") <>
                    implCode <>
                    BLU.fromString ")"

            Pretty ->
                T.writeFile (to "pure.raw.py") $
                    T.pack ("(" ++ escape mp ++ ",") <>
                      codePretty implCode <>
                    T.pack ")"
            
            Compressed -> do
              source <- mkEntrySelector "source"
              srcPath <- mkEntrySelector "srcpath"
              createArchive (to "pure.zip.py") $ do
                  addEntry BZip2 (toStrict $ serialize implCode) source
                  addEntry BZip2 (BSU.fromString mp) srcPath

        T.writeFile (to "pure.py") loaderCode
        return hasForeign

  let newModsToImport = map snd (moduleImports module')
  let newFModToAdd = S.fromList [mp | hasForeign]
  return (newModsToImport, newFModToAdd)

runToModulePath ::  [FilePath] -> [FilePath] -> P.ModuleName -> String
runToModulePath prefix suffix (P.ModuleName pns) =
    joinPath
      . (prefix++)
      . (++suffix)
      . List.map T.unpack
      . T.splitOn (T.pack ".")
      $ pns

runModuleName ::  [FilePath] -> [FilePath] -> P.ModuleName -> String
runModuleName prefix suffix (P.ModuleName pns) =
    List.intercalate "."
      . (prefix++)
      . (++suffix)
      . return
      . T.unpack
      $ pns

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


codePretty :: Doc PrettyTopdown -> T.Text
codePretty = renderStrict . layoutPretty defaultLayoutOptions

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
