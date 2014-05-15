module Main where

import           Automata.CFG
import           Automata.DTD
import           Automata.PDA
import           Control.Monad (when)
import           Data.Functor.Foldable (Fix(..))
import           Data.Monoid ((<>))
import           System.Environment (getArgs)
import           System.Exit (exitFailure)
import           Verifier.Lexer
import           Verifier.Parser

main :: IO ()
main = do
  args <- getArgs
  when (length args /= 2) $ putStrLn "./dtd-verify <spec> <input>" >> exitFailure
  [spec, input] <- mapM readFile args

  let cfg = dtdToCfg . Fix . parseDTD . map fst $ alexScanTokens spec
      result = simulate cfg input

  print $ if result then "Valid" else "Invalid"
