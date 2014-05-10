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
  | AOption AID AID
  | ALeastZero AID
  | ALeastOne AID
  | AData
  deriving (Eq, Functor, Show)
