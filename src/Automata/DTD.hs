{-# LANGUAGE DeriveFunctor #-}

module Automata.DTD
where

import Data.Functor.Foldable

type DocAST = Fix AST
type AID    = String

data AST t
  = ADocument AID [t]
  | AEntry AID [t]
  | AReference AID
  | AOption t t
  | ALeastZero t
  | ALeastOne t
  | AOptional t
  | AData
  deriving (Eq, Functor, Show)
