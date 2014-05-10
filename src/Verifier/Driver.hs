module Main where

import           Automata.DTD
import           Automata.PDA
import           Control.Monad (when)
import qualified Data.Map as M
import           Data.Monoid ((<>))
import qualified Data.Set as S
import           Verifier.Lexer
import           Verifier.Parser

main :: IO ()
main = do
  tokens <- fmap alexScanTokens getContents
  putStrLn $ "Parsed tokens: " <> show tokens
  print . parseDTD . map fst $ tokens
