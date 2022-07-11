module Data where

import Data.Map
import Data.Typeable
import LazyProofs
import qualified LazyProofs as Lib

data Type = StringT | IntT deriving (Eq, Show, Typeable, Read)

type Derivation = Lib.Derivation Fact

data TemplateType = TList (Term String) (Term String) (Term String) (Term [(Term String, Term String)])
  | TCreate (Term String) (Term String) (Term String)
  | TFilterList (Term String) (Term String) (Term String) (Term [(Term String, Term String)]) (Term String)
  deriving (Eq, Show, Typeable)

data Fact
  = Field (Term String)
  | Operator (Term String) (Term String)
  | Record (Term String)
  | Page (Term String)
  | Show (Term String)
  | Create (Term String)
  | Filter (Term String)
  | What (Term String) (Term String)
  | Where (Term String) (Term String)
  | PartOf (Term String) (Term String) (Term String)
  | Type (Term String) (Term String)
  | Location (Term String)
  | Entity (Term String) (Map (Term String) (Term String))
  | Query (Term String) (Term String) [Term String]
  | Query2 (Term String) (Term String) [Term String] (Term [Term String])
  | Attributes (Term String) (Term String) (Term [(Term String, Term String)]) (Term [(Term String, (Term String, Term String))])
  | Template TemplateType
  | WhatLocation (Term String) (Term String)
  | WhatRecord (Term String) (Term String)
  | WhatField (Term String) (Term String)
  | MultiAnd [Integer] (Term [String]) Fact
  deriving (Eq, Show, Typeable)
