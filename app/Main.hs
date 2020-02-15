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


import Language.PureScript.CodeGen.JS (moduleToJs)
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

opts = Options { optionsVerboseErrors = True
               , optionsNoComments = False
               , optionsCodegenTargets = S.empty
               }
                
parseOptsKey m = \case
    "--run": xs -> parseOptsKey (M.insert "run" "" m) xs
    "-h"   : xs -> parseOptsKey (M.insert "h" "" m) xs
    "-v"   : xs -> parseOptsKey (M.insert "v" "" m) xs
    k      : xs -> Left k
    []          -> Right m

parseOptVal k m = \case
    v : xs -> parseOptsKey (M.insert k v m) xs
    []     -> Left k


main :: IO ()
main = do
    opts <- getArgs
    case parseOptsKey M.empty opts of
        Left k -> putStrLn (printf "Unknown option (%s)." k) >> exitFailure
        Right v -> wain v

wain m
    | "--run" `M.member` m = return ()
    | otherwise = exitFailure

transpile :: FilePath -> FilePath -> IO ()
transpile baseOutpath jsonFile = do
  jsonText <- T.decodeUtf8 <$> B.readFile jsonFile
  let module' = jsonToModule $ parseJson jsonText

  case State.runStateT (runSTEither (runSupplyT 5 (moduleToJs module' Nothing))) opts of
      Left e -> putStrLn (show (e :: MultipleErrors)) >> exitFailure
      Right ((asts, _), _) -> do
        let
            implementation = T.unlines [doc2Text (finally ast :: Doc Py) | ast <- asts]
            outPath = runToModulePath $ moduleName module'
            implPath = outPath </> "impl.py"
        createDirectoryIfMissing True outPath
        T.writeFile implPath implementation


doc2Text :: Doc ann -> T.Text
doc2Text = renderStrict . layoutPretty defaultLayoutOptions

interfaceFileName :: String -> FilePath
interfaceFileName mn = mn <> ".py"

runToModulePath :: P.ModuleName -> String
runToModulePath (P.ModuleName pns) = joinPath . map T.unpack $ (P.runProperName <$> pns)

parseJson :: Text -> Value
parseJson text
  | Just fileJson <- decode . L.encodeUtf8 $ L.fromStrict text = fileJson
  | otherwise = error "Bad json"

jsonToModule :: Value -> Module Ann
jsonToModule value =
  case parse moduleFromJSON value of
    Success (_, r) -> r
    _ -> error "Bad corefn"

