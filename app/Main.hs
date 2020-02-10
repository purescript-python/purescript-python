module Main where

import PsPy
import System.Environment
import System.Exit
import qualified Data.Map as M
import Text.Printf (printf)


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
        


