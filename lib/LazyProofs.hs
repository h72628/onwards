{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE StandaloneDeriving #-}
module LazyProofs where

import Control.Applicative
  ( Alternative (empty, (<|>)),
  )
import Control.Monad.Logic (LogicT, ifte)
import Control.Monad.State
    ( MonadState(put, get), MonadTrans(lift), StateT )
import Data.Functor.Identity ( Identity )
import Data.Map (Map)
import Data.List (union)
import Data.Typeable (Typeable)

--------------------------
-- Data types for rules --
--------------------------

cast :: (Show s, Ord s) => Term s -> Term2
cast (Value v) = MkValue v
cast (Var v) = Vari v

data Term s = Value s | Var Integer
  deriving (Eq, Ord, Typeable)


instance Show s => Show (Term s) where
  show (Var s) = "x" ++ show s
  show (Value s) = show s

data Term2 = forall s . (Show s, Eq s, Ord s) => MkValue s
          | Vari Integer


instance Show Term2 where
  show (MkValue s) = "Value " ++ show s
  show (Vari v0)  = show "Var " ++ show v0

instance Eq Term2 where
  (==) (MkValue s) (MkValue s') = show s == show s'
  (==) (Vari v0) (Vari v1) = v0 == v1
  (==) _ _ = False

type Sequent a = ([Integer], [a], a)

type StateType a = (Integer, Map String (Sequent a), Map Integer Term2)

type Derivation a = LogicT (StateT (StateType a) Identity) (Sequent a)

 ---------------------
 -- State handling  --
 ---------------------

fresh :: LogicT (StateT (StateType a) Identity) Integer
fresh = do
  (m, mem, vars) <- lift get
  lift $ put (m + 1, mem, vars)
  return m

-----------------------
-- Utility functions --
-----------------------


list_union :: Eq a => [[a]] -> [a]
list_union = foldl union []

choose :: [a] -> LogicT b a
choose = foldr ((<|>) . pure) empty

axiom :: (a -> Bool) -> [a] -> Derivation a
axiom f l = do
  a <- choose (filter f l)
  pure ([], [], a)

lazy :: [Integer] -> a -> Derivation a -> Derivation a
lazy vars b r = ifte r pure (pure (vars, [b], b))

lazy2 :: (Bool -> Derivation a) -> Derivation a
lazy2 r = r True
