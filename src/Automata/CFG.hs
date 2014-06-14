module Automata.CFG
where

import           Automata.DTD
import           Data.Functor.Foldable
import qualified Data.Map as M
import           Data.Monoid ((<>))
import qualified Data.Set as S

data Terminal
  = Epsilon
  | T Char
  | DataSequence
  deriving (Eq, Ord)

instance Show Terminal where
  show (Epsilon) = "eps"
  show (T c) = "'" <> [c] <> "'"
  show (DataSequence) = "data"

type ProdHead = String
type TerminalList = [Either Terminal ProdHead]
type Production = (ProdHead, [TerminalList])

-- Productions and terminals
data CFG
  = CFG [Production]
  deriving (Eq, Ord)

show' (h, list) = map (\l -> h <> ": " <> unwords (map show'' l)) list
show'' (Left t) = show t
show'' (Right t) = t

instance Show CFG where
  show (CFG prods) = unlines $ concatMap show' prods

dtdToCfg :: DocAST -> CFG
dtdToCfg (Fix (ADocument _ entries)) = CFG prods
  where
  prods = concatMap (fst . convert) entries
  convert :: DocAST -> ([Production], TerminalList)
  convert (Fix (AEntry name opts)) = (prods, [])
    where
    prods = [(name, [begin <> list <> end])] <> concatMap fst subResult
    begin = map Left $ [T '<'] <> map T name <> [T '>']
    end   = map Left $ [T '<'] <> [T '/']  <> map T name <> [T '>']
    list = concatMap snd subResult
    subResult = map convert opts

  convert (Fix (AReference name)) = ([], [Right name])

  convert (Fix AData) = ([], [Left DataSequence])

  -- option handled as multiple production bodies.
  convert (Fix (AOption t1 t2)) = (p1 <> p2 <> [this], [Right thisName])
    where
    (p1, [Right c1]) = convert t1
    (p2, [Right c2]) = convert t2
    thisName         = c1 <> "Or" <> c2
    this             = (thisName, [[Right c1], [Right c2]])

  -- + handled as a new production
  convert (Fix (ALeastOne t)) = (p1 <> [this], [Right thisName])
    where
    (p1, [Right c1]) = convert t
    thisName         = "LeastOne" <> c1
    this             = (thisName, [[Right c1], [Right c1, Right thisName]])

  -- * handled as a new production
  convert (Fix (ALeastZero t)) = (p1 <> [this], [Right thisName])
    where
    (p1, [Right c1]) = convert t
    thisName         = "LeastZero" <> c1
    this             = (thisName, [[Left Epsilon], [Right c1, Right thisName]])

  -- ? handled as optionally empty
  convert (Fix (AOptional t)) = (p1 <> [this], [Right thisName])
    where
    (p1, [Right c1]) = convert t
    thisName         = "Optional" <> c1
    this             = (thisName, [[Left Epsilon], [Right c1]])
