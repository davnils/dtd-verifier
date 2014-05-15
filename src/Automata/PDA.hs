module Automata.PDA
where

import           Automata.CFG
import           Automata.DTD
import           Data.Functor.Foldable (cata)
import qualified Data.Map as M
import           Data.Maybe (fromJust)
import           Data.Semigroup ((<>), Semigroup)
import qualified Data.Set as S


-- Implementation of direct CFG simulation
-- The simulation progresses by expanding a variable on the stack (non-deterministically)
-- and matching the corresponding terminals in the input.
-- Any mismatched symbols terminates the computation.
-- This continus until one execution path succeeds.

-- The (SINGLE) start production is assumed to be (head prods)
simulate :: CFG -> String -> IO Bool
simulate (CFG prods) input = do
  putStrLn $ "contents of start prod: " <> show startProd
  putStrLn $ "contents of start inpu: " <> show input
  return $ go startProd input
  where
  startProd = head . snd $ head prods

  -- parse data if available (including spaces etc)
  go (Left DataSequence:xs) cs = go xs $ dropWhile (/= '<') cs

  -- ignore space and newline
  go cs (' ':xs) = go cs xs
  go cs ('\n':xs) = go cs xs
  go cs ('\t':xs) = go cs xs

  go (Left Epsilon:xs) cs = go xs cs
  go (Left (T sym):xs) (c:cs) = if sym == c then go xs cs else False

  go [] [] = True
  go [] _ = False
  go _ [] = False

  -- survey all expansions, push onto stack
  go (Right prod:xs) cs = or $ map (\cand -> go (cand <> xs) cs) cands
    where
    cands = concat $ map snd $ filter (\(h, _) -> h == prod) prods
