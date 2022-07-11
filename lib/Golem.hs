{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Golem where

import Control.Applicative ((<|>))
import Control.Monad (foldM)
import Control.Monad.Logic (MonadTrans (lift), guard, ifte, observeAllT)
import Control.Monad.State.Class (put)
import Control.Monad.Trans.State (get, runStateT)
import Data
    ( Fact(..), TemplateType(TFilterList, TList, TCreate), Derivation )
import Data.Functor.Identity (runIdentity)
import Data.List (union)
import qualified Data.Map as M
import LazyProofs (Term (..), Term2 (..), cast, choose, fresh, lazy, lazy2, list_union)
import qualified LazyProofs as Lib

-- Axioms
ruleShow :: [Fact] -> Derivation
ruleShow l = do
  v@(Show _) <- choose l
  pure ([], [], v)

ruleFilter :: [Fact] -> Derivation
ruleFilter l = do
  v@(Filter _) <- choose l
  pure ([], [], v)

ruleRecord :: [Fact] -> Derivation
ruleRecord l =
  ifte
    ( do
        v@(Record _) <- choose l
        pure ([], [], v)
    )
    pure
    ( do
        (Entity e _) <- choose l
        pure ([], [], Record e)
    )

rulePage :: [Fact] -> Derivation
rulePage l = do
  v@(Page _) <- choose l
  pure ([], [], v)

rulePartOf :: [Fact] -> Term String -> Term String -> Derivation
rulePartOf l s e = do
  v@(PartOf s' e' _) <- choose l
  guard (s == s' && e == e')
  pure ([], [], v)

ruleField :: [Fact] -> Derivation
ruleField l = do
  s@(Field _) <- choose l
  pure ([], [], s)

ruleType :: [Fact] -> Term String -> Derivation
ruleType l s = do
  t@(Type s' _) <- choose l
  guard (s == s')
  pure ([], [], t)

-- Rules
hasObject :: [Fact] -> Term String -> Term String -> Lib.Derivation Fact
hasObject l s e = do
  What s' e' <- choose l
  guard (s == s' && e == e')
  pure ([], [], What s' e')

rule7 :: [Fact] -> Term String -> Bool -> Derivation
rule7 l s deferred = do
  var0 <- fresh
  (v1, h1, Record mv_e) <- if deferred then lazy [var0] (Record (Var var0)) $ ruleRecord l else ruleRecord l
  (v2, h2, What _ ho_e) <- if deferred then lazy [] (What s mv_e) $ hasObject l s mv_e else hasObject l s mv_e
  Just _ <- unify (cast mv_e) (cast ho_e)
  pure (list_union [v1, v2], list_union [h1, h2], WhatRecord s ho_e)

rule8 :: [Fact] -> Term String -> Bool -> Derivation
rule8 l s deferred = do
  var0 <- fresh
  (v1, h1, Field sv_f) <- if deferred then lazy [var0] (Field (Var var0)) $ ruleField l else ruleField l
  (v2, h2, What _ ho_f) <- if deferred then lazy [] (What s sv_f) $ hasObject l s sv_f else hasObject l s sv_f
  Just _ <- unify (cast sv_f) (cast ho_f)
  pure (list_union [v1, v2], list_union [h1, h2], WhatField s ho_f)

hasPage :: [Fact] -> Term String -> Term String -> Lib.Derivation Fact
hasPage l s p = do
  Where s' p' <- choose l
  guard (s == s' && p == p')
  pure ([], [], Where s' p')

rule6 :: [Fact] -> Term String -> Bool -> Derivation
rule6 l s deferred = do
  (v1, h1, Location l_p) <- location l deferred
  (v2, h2, Where _ hp_l) <- if deferred then lazy [] (Where s l_p) $ hasPage l s l_p else hasPage l s l_p
  Just _ <- unify (cast l_p) (cast hp_l)
  pure (list_union [v1, v2], list_union [h1, h2], WhatLocation s l_p)

--    Page(P)
--  -------------- (2)
--    Location(P)
--
rule5 :: [Fact] -> Bool -> Derivation
rule5 l deferred = do
  var0 <- fresh
  (v1, h1, Page p) <- if deferred then lazy [var0] (Page (Var var0)) $ rulePage l else rulePage l
  pure (v1, h1, Location p)

location :: [Fact] -> Bool -> Derivation
location l deferred =
  ifte
    (rule5 l False)
    pure
    ( rule5 l deferred
        <|> ( do
                p@(Location _) <- choose l
                pure ([], [], p)
            )
    )

--
--    Multivalue(P)
--  ----------------- (5)
--    Entity(P,{})
--
rule9 :: [Fact] -> Derivation
rule9 l =
  ifte
    ( do
        e@(Entity _ _) <- choose l
        pure ([], [], e)
    )
    pure
    ( do
        Record e <- choose l
        pure ([], [], Entity e M.empty)
    )

rule10 :: [Fact] -> Derivation
rule10 l = do
  PartOf _ e' s' <- choose l
  Field s <- choose l
  guard (s == s')
  var0 <- fresh
  (v2, h2, Type s'' t) <- lazy [var0] (Type s (Var var0)) $ ruleType l s
  guard (s == s'')
  (v1, h1, Entity e a) <- rule9 l
  guard (e == e')
  pure (list_union [v1, v2], list_union [h1, h2], Entity e (M.insert s t a))

entity :: [Fact] -> Derivation
entity l = do
  start@(_, _, Entity e _) <- rule9 l
  state@(_, mem, _) <- lift get
  let key = show e

  ifte
    (guard (M.member key mem))
    (\_ -> pure $ mem M.! key)
    ( do
        let (attrs, (curr, _, assoc)) = runIdentity $ runStateT (observeAllT (rule10 l)) state
        let entity_attrs = filter (\(_, _, Entity e' _) -> e == e') attrs
        let ent = foldl (\(v0, h0, Entity e0 a0) (v1, h1, Entity _ a1) -> (v0 ++ v1, h0 ++ h1, Entity e0 (M.union a0 a1))) start entity_attrs
        lift $ put (curr, M.insert key ent mem, assoc)
        pure ent
    )

rule11 :: [Fact] -> Term String -> Bool -> Derivation
rule11 l a deferred = do
  Show show_a <- if deferred then ifte (ruleShow l >>= \(_, _, Show show_a) -> guard (a == show_a) >> pure (Show show_a)) pure (pure $ Show a) else choose l
  guard (show_a == a)
  (v1, h1, WhatRecord _ mv_e) <- rule7 l a deferred
  (v2, h2, Entity ent _) <- if deferred then ifte (entity l >>= \ent@(_, _, Entity e' _) -> guard (mv_e == e') >> pure ent) pure (pure ([], [], Entity mv_e M.empty)) else entity l
  guard (mv_e == ent)
  pure (list_union [v1, v2], list_union [h1, h2], Query (Value $ "q" ++ show a) a [ent])

rule12 :: [Fact] -> Term String -> Bool -> Derivation
rule12 l a deferred = do
  Filter filter_a <- if deferred then ifte (ruleFilter l >>= \(_, _, Filter filter_a) -> guard (a == filter_a) >> pure (Filter filter_a)) pure (pure $ Filter a) else choose l
  guard (filter_a == a)
  (v1, h1, WhatRecord _ mv_e) <- rule7 l filter_a deferred
  (v2, h2, Entity ent _) <- if deferred then ifte (entity l >>= \ent@(_, _, Entity e' _) -> guard (mv_e == e') >> pure ent) pure (pure ([], [], Entity mv_e M.empty)) else entity l
  guard (mv_e == ent)
  (v3, h3, WhatField _ field) <- rule8 l filter_a deferred
  pure (list_union [v1, v2, v3], list_union [h1, h2, h3], Query2 (Value $ "q" ++ show a) a [ent] (Value [field]))

rule16 :: Term String -> Term String -> [Fact] -> Bool -> Derivation
rule16 s e l deferred = do
  (v1, h1, Entity e' _) <- if deferred then ifte (entity l >>= \ent@(_, _, Entity e' _) -> guard (e == e') >> pure ent) pure (pure ([], [], Entity e M.empty)) else entity l
  guard (e == e')
  curr_state <- lift get
  let (composed_sequents, next_state) = runIdentity $ runStateT (observeAllT $ rulePartOf l s e) curr_state
  lift $ put next_state
  if null composed_sequents
    then do
      var0 <- fresh
      var1 <- fresh
      var2 <- fresh
      var3 <- fresh
      var4 <- fresh
      pure (v1 `union` [var0, var1, var2, var3, var4], h1 `union` [MultiAnd [var0] (Var var1) (PartOf s e (Var var0)), MultiAnd [var2, var3] (Var var4) (Operator (Var var2) (Var var3))], Attributes s e (Var var1) (Var var4))
    else do
      let (c_v, c_h, composed) = foldl (\(v, h, f) (v', h', PartOf _ c_e attr) -> (v `union` v', h `union` h', (attr, qualifiedName c_e attr) : f)) ([], [], []) composed_sequents
      pure (v1 `union` c_v, h1 `union` c_h, Attributes s e (Value composed) (Value []))

rule20 :: [Fact] -> Derivation
rule20 l = do
  Show show_s <- choose l
  (v1, h1, WhatRecord mv_s mv_e) <- lazy2 $ rule7 l show_s
  (v2, h2, WhatLocation loc_s loc_l) <- lazy2 $ rule6 l show_s
  (v3, h3, Query agg_n agg_s [agg_e]) <- lazy2 $ rule11 l show_s
  (v4, h4, Attributes att_s att_e att_a _) <- lazy2 $ rule16 show_s agg_e l
  Just _ <- unifyMultiple show_s [mv_s, loc_s, agg_s, att_s]
  Just _ <- unifyMultiple mv_e [agg_e, att_e]
  pure (list_union [v1, v2, v3, v4], list_union [h1, h2, h3, h4], Template (TList show_s loc_l agg_n att_a))

rule22 :: [Fact] -> Derivation
rule22 l = do
  Create create_s <- choose l
  (v1, h1, WhatRecord mv_s mv_e) <- lazy2 $ rule7 l create_s
  (v2, h2, WhatLocation loc_s loc_l) <- lazy2 $ rule6 l create_s
  Just _ <- unifyMultiple create_s [mv_s, loc_s]
  pure (list_union [v1, v2], list_union [h1, h2], Template (TCreate create_s loc_l mv_e))

rule23 :: [Fact] -> Derivation
rule23 l = do
  Filter filter_s <- choose l
  (v1, h1, WhatField sv_s sv_f) <- lazy2 $ rule8 l filter_s
  (v2, h2, WhatRecord mv_s mv_e) <- lazy2 $ rule7 l filter_s
  (v3, h3, WhatLocation loc_s loc_l) <- lazy2 $ rule6 l filter_s
  (v4, h4, Query agg_n agg_s [agg_e]) <- lazy2 $ rule11 l filter_s
  (v5, h5, Attributes att_s att_e att_a _) <- lazy2 $ rule16 filter_s agg_e l
  Just _ <- unifyMultiple mv_e [agg_e, att_e]
  Just _ <- unifyMultiple filter_s [mv_s, loc_s, sv_s, att_s, agg_s]
  -- guard (sv_f `elem` att_a)
  pure (list_union [v1, v2, v3, v4, v5], list_union [h1, h2, h3, h4, h5], Template (TFilterList filter_s loc_l agg_n att_a sv_f))

unifyMultiple v0 vs = do
  (_, _, assoc) <- lift get
  foldM (foldf v0) (Just assoc) vs
  where
    foldf v0 val v = case val of
      Just _ -> unify (cast v0) (cast v)
      Nothing -> return Nothing

unify var@(Vari v0) t = do
  (counter, mem, assoc) <- lift get
  if M.member v0 assoc
    then let val = assoc M.! v0 in if val /= t then unify val t else return $ Just assoc
    else do
      if var /= t
        then do
          lift $ put (counter, mem, M.insert v0 t assoc)
          return $ Just (M.insert v0 t assoc)
        else return $ Just assoc
unify v0@(MkValue _) v1@(MkValue _) = do
  (_, _, assoc) <- lift get
  return $ if v0 == v1 then Just assoc else Nothing
unify v@(MkValue _) var@(Vari _) = unify var v

qualifiedName :: Term String -> Term String -> Term String
qualifiedName (Value v) (Value v') = Value $ v ++ "." ++ v'
qualifiedName (Var v) (Var v') = Value $ show v ++ "." ++ show v'
qualifiedName (Value v) (Var v') = Value $ v ++ "." ++ show v'
qualifiedName (Var v) (Value v') = Value $ show v ++ "." ++ v'
