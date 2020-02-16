module Main where
import System.Exit
import System.Environment
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>), joinPath, takeFileName)

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

import Language.PureScript.CoreFn
import Language.PureScript.CoreFn.FromJSON
import Language.PureScript.Errors (MultipleErrors)
import qualified Language.PureScript as P
import qualified Language.PureScript.Hierarchy as P
import Language.PureScript.Options (Options(..))


import Language.PureScript.CodeGen.Py (moduleToJS)
import Language.PureScript.CodeGen.Py.Printer (Py, bindSExpr)
import Language.PureScript.CodeGen.Py.Eval (finally)
import Language.PureScript.CodeGen.Py.Common (escape)

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
        ["--foreign-top", foreignBaseDir, "--out-top", baseOutDir, "--corefn", jsonFile] ->
            cg foreignBaseDir baseOutDir jsonFile
        _ ->  putStrLn "Malformed options, expect form --foreign-top <dir0> --out-top <dir1> --corefn <xxx.json>." >> exitFailure

cg :: FilePath -> FilePath -> FilePath -> IO ()
cg foreignBaseDir baseOutDir jsonFile = do
  jsonText <- T.decodeUtf8 <$> B.readFile jsonFile
  let module' = jsonToModule $ parseJson jsonText
      mn      = moduleName module'
      package = takeFileName baseOutDir
  case flip State.runStateT defaultOpts . runSTEither .runSupplyT 5 $
        moduleToJS module' (T.pack package) of
      Left e -> print (e :: MultipleErrors) >> exitFailure
      Right (((hasForeign, ast), _), _) -> do
        let
            implCode = legalizedCodeGen (finally ast)
            outDir = runToModulePath baseOutDir mn
            ffiDir = runToModulePath foreignBaseDir mn
            entryPath = outDir </> "__init__.py"
            implSrcPath = outDir </> "purescript_impl.src.py"
            implLoaderPath = outDir </> "purescript_impl.py"
            foreignSrcPath = ffiDir <> ".py"
            foreignDestPath = outDir </> "purescript_foreign.py"

        createDirectoryIfMissing True outDir
        T.writeFile entryPath T.empty
        T.writeFile implSrcPath implCode
        T.writeFile implLoaderPath loaderCode
        when hasForeign $ do
            foreignCode <- T.readFile foreignSrcPath
            T.writeFile foreignDestPath foreignCode

runToModulePath :: FilePath -> P.ModuleName -> String
runToModulePath base (P.ModuleName pns) =
        joinPath . (base:) . map T.unpack $ (P.runProperName <$> pns)

parseJson :: Text -> Value
parseJson text
  | Just fileJson <- decode . L.encodeUtf8 $ L.fromStrict text = fileJson
  | otherwise = error "Bad json"

jsonToModule :: Value -> Module Ann
jsonToModule value =
  case parse moduleFromJSON value of
    Success (_, r) -> r
    _ -> error "Bad corefn"



doc2Text :: Doc ann -> T.Text
doc2Text = renderStrict . layoutPretty defaultLayoutOptions

legalizedCodeGen :: Doc Py -> Text
legalizedCodeGen sexpr =
    T.unlines
      [
        T.pack "from py_sexpr.terms import *"
      , T.pack "from py_sexpr.stack_vm.emit import module_code"
      , doc2Text (bindSExpr "res" sexpr)
      , T.pack "res = module_code(res)"
      ]

loaderCode :: Text
loaderCode =
    T.unlines . map T.pack $
      [
        "from purescripto import LoadPureScript"
      , "_py__ = globals()"
      , "_ps__ = LoadPureScript(__name__, __file__)"
      , "__all__ = list(__ps__.exports)"
      , "__py__.update(__ps__.exports)"
      ]      
