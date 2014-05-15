module Main where

import           Automata.CFG
import           Automata.DTD
import           Automata.PDA
import           Control.Monad (when)
import           Data.Functor.Foldable
import qualified Data.Map as M
import           Data.Monoid ((<>))
import qualified Data.Set as S
import           System.Environment (getArgs)
import           Verifier.Lexer
import           Verifier.Parser

main :: IO ()
main = do
  [f1, f2] <- getArgs

  tokens <- fmap alexScanTokens (readFile f1)
  putStrLn $ "Parsed tokens: " <> show tokens

  let cfg = dtdToCfg . Fix . parseDTD . map fst $ tokens

  putStrLn "Create CFG"
  print cfg

  putStrLn "Running simulation"
  input <- readFile f2
  simulate cfg input >>= print
