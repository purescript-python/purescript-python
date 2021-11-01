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
import qualified Data.Map as M

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


import Language.PureScript.CodeGen.Diana (moduleToJS)
import Language.PureScript.CodeGen.Diana.Serializer
import Language.PureScript.CodeGen.Diana.Eval (finally, EvalJS)
import Control.Monad.Supply

import Data.Text.Prettyprint.Doc.Render.Text (renderStrict)
import Data.Text.Prettyprint.Doc (Doc, layoutPretty, defaultLayoutOptions)

import Control.Monad (when)
import Control.Monad.Reader (MonadReader(..))
import qualified Control.Monad.State as State

import Monads.STEither
import Codec.Archive.Zip
import StringEscape (escape)
import Control.Monad.State (State)
import Language.PureScript.CodeGen.Diana.Serializer (runDoc)

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
    let baseOutDir = "output"
    let ffiDepPath = "ffi-deps"
    let moduleParts = "Main"

    ffiDeps <- do
      
      fixPointCG
        baseOutDir
        S.empty
        (S.empty, [moduleNameFromString $ T.pack moduleParts])
  
    T.writeFile ffiDepPath (T.unlines $ map T.pack ffiDeps)


-- code generation for used modules
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

toStrict :: BL.ByteString -> B.ByteString
toStrict = B.concat . BL.toChunks

-- code generation for each  module
cg ::  FilePath -> P.ModuleName -> IO ([P.ModuleName], S.Set FilePath)
cg  baseOutDir coreFnMN = do
  pwd  <- getCurrentDirectory
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

  hasForeign <- case flip State.runStateT defaultOpts . runSTEither .runSupplyT 5 $ moduleToJS module' (T.pack package) of
      Left e -> print (e :: MultipleErrors) >> exitFailure
      Right (((hasForeign, ast), _), _) -> do
        let augmentedAST = everywhere (astSSToAbsPath pwd) ast
            outDir = runToModulePath [pwd, baseOutDir] [] mn
            to :: FilePath -> FilePath
            to = (outDir </>)
            implCode :: EvalJS (State (M.Map String Int) (Doc a)) => Doc a
            implCode = runDoc $ finally augmentedAST

        putStrLn $ "Codegen DianaScript for " ++ qualifiedMN
        createDirectoryIfMissing True outDir
        T.writeFile (to "@main.ran") $ codePretty implCode
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


codePretty :: Doc a -> T.Text
codePretty = renderStrict . layoutPretty defaultLayoutOptions
