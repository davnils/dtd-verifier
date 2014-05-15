module Automata.PDA
where

import           Automata.CFG
import           Automata.DTD
import           Data.Semigroup ((<>))

-- Implementation of direct CFG simulation
-- The simulation progresses by expanding a variable on the stack (non-deterministically)
-- and matching the corresponding terminals in the input.
-- Any mismatched symbols terminates the computation.
-- This continus until one execution path succeeds.

-- | The (SINGLE) start production is assumed to be (head prods)
simulate :: CFG -> String -> Bool
simulate (CFG prods) = go startProd
  where
  startProd = head . snd $ head prods

  -- parse data if available (including spaces etc)
  go (Left DataSequence:xs) cs = go xs $ dropWhile (/= '<') cs

  -- ignore space and newline
  go cs (' ':xs) = go cs xs
  go cs ('\n':xs) = go cs xs
  go cs ('\t':xs) = go cs xs

  -- terminate if reaching the end of either
  go [] [] = True
  go [] _ = False
  go _ [] = False

  go (Left Epsilon:xs) cs = go xs cs
  go (Left (T sym):xs) (c:cs) = sym == c && go xs cs

  -- survey all expansions, push onto stack
  go (Right prod:xs) cs = any (\cand -> go (cand <> xs) cs) cands
    where
    cands = concatMap snd $ filter (\(h, _) -> h == prod) prods
