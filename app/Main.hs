module Main where
import System.Exit
import System.Environment
import System.Directory (createDirectoryIfMissing, getCurrentDirectory)
import System.FilePath ((</>), joinPath, takeFileName, splitDirectories)

import Text.Printf (printf)
import qualified Data.Map as M

import Data.Aeson hiding (Options)
import Data.Aeson.Types hiding (Options)
import Data.Text (Text, pack, unpack)

import qualified Data.List as List
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as T

import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.Encoding as L
import qualified Data.ByteString as B
import qualified Data.Set as S


import Language.PureScript.CoreImp.AST (withSourceSpan, getSourceSpan, everywhere)
import qualified Language.PureScript.AST.SourcePos as SP

import Language.PureScript.CoreFn
import Language.PureScript.CoreFn.FromJSON
import Language.PureScript.Errors (MultipleErrors)
import Language.PureScript.Options (Options(..))
import qualified Language.PureScript as P
import qualified Language.PureScript.Hierarchy as P


import Language.PureScript.CodeGen.Py (moduleToJS)
import Language.PureScript.CodeGen.Py.Printer (Py, bindSExpr)
import Language.PureScript.CodeGen.Py.Eval (finally, takeSourceLoc)
import Language.PureScript.CodeGen.Py.Common (escape, SourceLoc(..))

import Control.Monad.Supply
import Control.Monad.Supply.Class

import Data.Text.Prettyprint.Doc.Render.Text (renderStrict)
import Data.Text.Prettyprint.Doc (Doc, layoutPretty, defaultLayoutOptions)

import Control.Monad (when)
import Control.Monad.Reader (MonadReader(..))
import qualified Control.Monad.State as State

import Monads.STEither

instance MonadReader Options (STEither Options MultipleErrors) where
    ask = STEither State.get
    -- TODO: implement a correct `local`
    local r m = m

defaultOpts =
    Options { optionsVerboseErrors = True
            , optionsNoComments = False
            , optionsCodegenTargets = S.empty
            }

main :: IO ()
main = do
    opts <- getArgs
    case opts of
        ["--out-top", baseOutDir, "--entry-corefn", jsonFile] ->
            cg baseOutDir jsonFile
        _ ->  putStrLn "Malformed options, expect form --foreign-top <dir0> --out-top <dir1> --corefn <xxx.json>." >> exitFailure


data Dependency
  = Dependency {
    getLibrary :: String
  , getVersion :: [Int]
  }

data ModuleSource
    = ModuleSource {
      getSource :: FilePath
    -- Nothing if this module is from current project
    , asDep     :: Maybe Dependency
    }


-- code generation for each  module
cg :: FilePath -> FilePath -> FilePath -> IO ()
cg foreignBaseDir baseOutDir jsonFile = do
  pwd      <- getCurrentDirectory
  jsonText <- T.decodeUtf8 <$> B.readFile jsonFile
  let module' = jsonToModule $ parseJson jsonText
      -- getting the module name
      mn      = moduleName module'
      -- getting the module path
      mp      = modulePath module'
      -- getting metadata of the module
      mspan   = takeSourceLoc (moduleSourceSpan module')
      -- name of the generated python package
      package = takeFileName baseOutDir
  case flip State.runStateT defaultOpts . runSTEither .runSupplyT 5 $
        moduleToJS module' (T.pack package) of
      Left e -> print (e :: MultipleErrors) >> exitFailure
      Right (((hasForeign, ast), _), _) -> do
        let
            implCode =
                legalizedCodeGen
                  (runModuleName package mn)
                  (joinPath [pwd, mp])
                  (finally $ astSSToAbsPath ast)

            outDir = runToModulePath baseOutDir mn
            implSrcPath = outDir </> "pure.src.py"
            implLoaderPath = outDir </> "pure.py"

        createDirectoryIfMissing True outDir
        T.writeFile implSrcPath implCode
        T.writeFile implLoaderPath loaderCode

runToModulePath ::  FilePath -> P.ModuleName -> String
runToModulePath base (P.ModuleName pns) =
        joinPath .
        (base:) . (++ ["pure"]) . map T.unpack $ (P.runProperName <$> pns)

runModuleName ::  FilePath -> P.ModuleName -> String
runModuleName base (P.ModuleName pns) =
        List.intercalate "." .
        (base:) . (++["pure"]) . map T.unpack $ (P.runProperName <$> pns)

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
