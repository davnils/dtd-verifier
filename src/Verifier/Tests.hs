module Search.Tests where

import Automata.DFA
import Automata.NFA
import Automata.RegExp

import           Control.Monad (foldM,forM,when)
import           Control.Monad.Trans (lift)
import qualified Control.Monad.Reader as R
import qualified Control.Monad.State as ST
import           Data.Foldable (forM_)
import           Data.Functor.Foldable (cata)
import           Data.List ((\\), foldl', intercalate, intersect, partition, sort, union)
import qualified Data.Map as M
import           Data.Maybe (fromJust)
import           Data.Semigroup ((<>), Semigroup)
import qualified Data.Set as S

test :: IO ()
test = do
  let binds  = [((0,1), 0), ((0,0), 1), ((1,1), 0), ((1,0), 3), ((3,0), 4), ((3,1), 5), ((4,0), 4), ((4,1), 5), ((5,0), 5), ((5,1), 5)]
      binds' = map (\((x, y), z) -> ((S.singleton x, head $ show y), S.singleton z)) binds
      dfa    = DFA (S.fromList "01") (M.fromList binds') (S.singleton 0) (S.singleton $ S.singleton 5) 0
      sep    = putStrLn "-----------------------------------------------"

  -- minDFA <- minimize dfa

  _ <- equivalent dfa

  print dfa
  sep
  -- print minDFA
  sep

test2 :: IO ()
test2 = do
  let binds  = [((0,0), 1), ((0,1), 5), ((1,0), 6), ((1,1), 2), ((2,0), 0), ((2,1), 2),
                ((3,0), 2), ((3,1), 6), ((4,0), 7), ((4,1), 5), ((5,0), 2), ((5,1), 6),
                ((6,0), 6), ((6,1), 4), ((7,0), 6), ((7,1), 2)]
      binds' = map (\((x, y), z) -> ((S.singleton x, head $ show y), S.singleton z)) binds
      dfa    = DFA (S.fromList "01") (M.fromList binds') (S.singleton 0) (S.singleton $ S.singleton 2) 0
      sep    = putStrLn "-----------------------------------------------"

  _ <- equivalent dfa
  minDFA <- minimize dfa
  -- print dfa
  -- sep
  print minDFA
  sep
  -- print $ map (\(x,y) -> (head $ S.toList x, head $ S.toList y)) (S.toList eq) -- map (map $ head . S.toList) eq
