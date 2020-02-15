module Main where
import System.Exit
import System.Environment
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>), joinPath)

import Text.Printf (printf)
import qualified Data.Map as M

import Data.Aeson hiding (Options)
import Data.Aeson.Types hiding (Options)

import Data.Text (Text, pack, unpack)
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
import Language.PureScript.CodeGen.Py.Printer (Py)
import Language.PureScript.CodeGen.Py.Eval (finally)

import Control.Monad.Supply
import Control.Monad.Supply.Class

import Data.Text.Prettyprint.Doc.Render.Text (renderStrict)
import Data.Text.Prettyprint.Doc (Doc, layoutPretty, defaultLayoutOptions)

import Control.Monad.Reader (MonadReader(..))
import qualified Control.Monad.State as State

import Monads.STEither
    
instance MonadReader Options (STEither Options MultipleErrors) where
    ask = STEither $ State.get
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
        ["--top-of-out", baseOutDir, "--corefn", jsonFile] ->
            cg baseOutDir jsonFile
        _ ->  putStrLn "Malformed options, expect form --top-of-out <dir1> --corefn <xxx.json>." >> exitFailure

cg :: FilePath -> FilePath -> IO ()
cg baseOutDir jsonFile = do
  jsonText <- T.decodeUtf8 <$> B.readFile jsonFile
  let module' = jsonToModule $ parseJson jsonText

  case flip State.runStateT defaultOpts . runSTEither .runSupplyT 5 $
        moduleToJS module' Nothing of
      Left e -> putStrLn (show (e :: MultipleErrors)) >> exitFailure
      Right ((ast, _), _) -> do
        let
            implementation = doc2Text (finally ast :: Doc Py)
            outDir = runToModulePath baseOutDir $ moduleName module'
            implPath = outDir </> "impl.py"
        createDirectoryIfMissing True outDir
        T.writeFile implPath implementation


doc2Text :: Doc ann -> T.Text
doc2Text = renderStrict . layoutPretty defaultLayoutOptions

interfaceFileName :: String -> FilePath
interfaceFileName mn = mn <> ".py"

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

