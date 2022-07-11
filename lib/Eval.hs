{-# LANGUAGE ScopedTypeVariables #-}
module Eval(replace, variables) where
import qualified Data.Map as M
import Data.Typeable ( Typeable, cast )
import Data
import LazyProofs ( list_union, Term(..) )

listSubst :: (Typeable s, Typeable b, Typeable a) => (a -> b) -> Term [a] -> Integer -> Term s -> Term [b]
listSubst f (Value l) _ _ = Value $ map f l
listSubst _ (Var v) v' val = subst (Var v) v' val

subst :: (Typeable s, Typeable ty) => Term ty -> Integer -> Term s -> Term ty
subst (Var v) var (Value val)
  | c <- cast val,
    v == var = case c of Just (z :: ty) -> Value z; _ -> Var v
  | otherwise = Var v
subst (Var v) var (Var v') = if v == var then Var v' else Var v
subst v _ _ = v

replace :: Typeable s => Term s -> Term s -> Fact -> Fact
replace (Var var) val (Field v) = Field (subst v var val)
replace (Var var) val (Record v) = Record (subst v var val)
replace (Var var) val (Show v) = Show (subst v var val)
replace (Var var) val (Create v) = Create (subst v var val)
replace (Var var) val (Filter v) = Filter (subst v var val)
replace (Var var) val (What v v') = What (subst v var val) (subst v' var val)
replace (Var var) val (Where v v') = Where (subst v var val) (subst v' var val)
replace (Var var) val (PartOf s e a) = PartOf (subst s var val) (subst e var val) (subst a var val)
replace (Var var) val (Type a t) = Type (subst a var val) (subst t var val)
replace (Var var) val (Location l) = Location (subst l var val)
replace (Var var) val (Entity e attrs) = Entity (subst e var val) (M.map (\a -> subst a var val) $ M.mapKeys (\k -> subst k var val) attrs)
replace (Var var) val (Query n s e) = Query (subst n var val) (subst s var val) (map (\t -> subst t var val) e)
replace (Var var) val (Attributes s e attrs comp) = Attributes (subst s var val) (subst e var val) (listSubst (\(a, n) -> (subst a var val, subst n var val)) attrs var val) (listSubst (\(a, (f, n)) -> (subst a var val, (subst f var val, subst n var val))) comp var val)
replace (Var var) val (Template (TList s l a args)) = Template $ TList (subst s var val) (subst l var val) (subst a var val) (subst args var val)
replace (Var var) val (Template (TCreate s l e)) = Template $ TCreate (subst s var val) (subst l var val) (subst e var val)
replace (Var var) val (Template (TFilterList s l a args filter)) = Template $ TFilterList (subst s var val) (subst l var val) (subst a var val) (subst args var val) (subst filter var val)
replace (Var var) val (WhatLocation s l) = WhatLocation (subst s var val) (subst l var val)
replace (Var var) val (Page p) = Page (subst p var val)
replace (Var var) val (WhatRecord s e) = WhatRecord (subst s var val) (subst e var val)
replace (Var var) val (WhatField s f) = WhatField (subst s var val) (subst f var val)
-- check if this is correct!
replace (Var var) val (MultiAnd v l f) = MultiAnd v (subst l var val) f
replace (Var var) val (Operator f fun) = Operator (subst f var val) (subst fun var val)
replace (Value _) _ f = f

vars :: Term s -> [Integer]
vars (Value _) = []
vars (Var v) = [v]

listVars :: (a -> [Integer]) -> Term [a] -> [Integer]
listVars f (Value l) = list_union $ map f l
listVars _ (Var v) = [v]

templateVariables :: TemplateType -> [Integer]
templateVariables (TList s l a attrs) = list_union [vars s, vars l, vars a,  listVars (\(a, n) -> list_union [vars a, vars n]) attrs]
templateVariables (TFilterList s l a attrs filter) = list_union [vars s, vars l, vars a, listVars (\(a, n) -> list_union [vars a, vars n]) attrs, vars filter]
templateVariables (TCreate s l e) = list_union $ map vars  [s, l, e]

variables :: Fact -> [Integer]
variables (Field v) = vars v
variables (Record v) = vars v
variables (Show v) = vars v
variables (Create v) = vars v
variables (Filter v) = vars v
variables (What v v') = list_union $ map vars  [v, v']
variables (Where v v') = list_union $ map vars  [v, v']
variables (PartOf s e a) = list_union $ map vars  [s, e, a]
variables (Type a t) = list_union $ map vars  [a, t]
variables (Location l) = vars l
variables (Entity e attrs) = list_union [vars e, list_union (map vars $ M.elems attrs), list_union (map vars $ M.keys attrs)]
variables (Query n s e) = list_union [vars n, vars s, list_union (map vars e)]
variables (Attributes s e attrs comp) = list_union [vars s, vars e, listVars (\(a, n) -> list_union [vars a, vars n]) attrs, listVars (\(a, (f, n)) -> list_union [vars a, vars f, vars n]) comp]
variables (Template t) =  templateVariables t
variables (WhatLocation s l) = list_union $ map vars  [s, l]
variables (Page p) = vars p
variables (WhatRecord s e) = list_union $ map vars  [s, e]
variables (WhatField s f) = list_union $ map vars  [s, f]
variables (Operator f fun) = list_union [vars f, vars fun]
variables (MultiAnd _ var fact) = list_union [vars var, variables fact]
