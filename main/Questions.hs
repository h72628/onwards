{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
module Questions where
import Data ( Fact )
import qualified Data.Map as M
import LazyProofs
    ( StateType, Sequent, Term(Value, Var), list_union )
import Data.List ( find, (\\), union )
import Text.Show.Pretty ( pPrint, ppShow )
import Eval ( replace, variables )
import System.IO ( hFlush, stdout )
import Golem ( entity, rule20, rule22, rule23 )
import Control.Monad.Identity ( Identity(runIdentity) )
import Control.Monad.State ( StateT(runStateT) )
import Control.Monad.Logic ( observeAllT )



infer :: Monad m => StateType Fact -> [Fact] -> m (StateType Fact, [Sequent Fact])
infer state facts = do
  let rules = map (\r -> r facts) [entity, rule20, rule22, rule23]
  return $ foldl (\(st, res) rule -> let (res1, st1) = runIdentity $ runStateT (observeAllT rule) st in (st1, res `union` res1)) (state, []) rules

loop :: [Fact] -> [Fact] -> IO ([a1], [a2], [Fact])
loop facts rejected = do
  -- Run inference rules
  ((_, _, assoc), sequents) <- infer (0, M.empty, M.empty) facts
  -- Ignore any sequents which contain previously rejected hypotheses
  let validSequents = filter (\(_, h, _) -> not $ any (`elem` rejected) h) sequents
  if null validSequents -- If all sequents have been rejected then no components can be inferred
    then do return ([], [], [])
    else do
      let components = map (\(_, _, c) -> c) validSequents
      let questions = any (\(v, h, _) -> not (null v) || not (null h)) validSequents
      if not questions
        then do
          return ([], [], components)
        else do
          pPrint validSequents
          pPrint assoc
          -- Select the first sequent with questions for the user
          let (Just (v, h, _)) = find (\(v, h, _) -> not (null v && null h)) validSequents
          -- Ask questions
          (no, nfacts) <- askLoop v h facts components
          pPrint nfacts
          -- Run inference loop again with the new facts
          loop nfacts (rejected `union` no)


askVariable v hs facts components = do
  putStrLn ""
  putStrLn ("This variable affects the following hypotheses: " ++ ppShow ref)
  input <- prompt ("Enter value for variable " ++ show v ++ ":")
  if input == ""
    then return (hs, facts)
    else do
      -- Replace variable with value in all hypotheses
      let nhs = map (replace (Var v) (Value input)) hs
      -- Select hypotheses without variables
      let nfacts = filter (null . variables) nhs
      return (nhs \\ nfacts, nfacts `union` facts)
  where
    ref = filter (elem v . variables) (hs `union` facts)

askFact :: Fact -> [Fact] -> [Fact] -> IO Bool
askFact h hs facts = do
  input <- prompt ("Is " ++ show h ++ " true? (y/n)")
  let val = input
  if val == "n"
    then do
      return False
    else
      if val == "y"
        then do
          return True
        else do askFact h hs facts

askLoop :: [Integer] -> [Fact] -> [Fact] -> [Fact] -> IO ([Fact], [Fact])
askLoop vs hs facts components = do
  -- pPrint facts
  (no, f0) <- askFacts [] vs (filter (null . variables) hs) facts
  f1 <- if null no then askVariables vs hs f0 components else return []
  return (no, list_union [f0, f1])
  where
    askVariables (v : vs) hs facts components = do
      (nhs, nfacts) <- askVariable v hs facts components
      -- close the loop if the answer introduces a new fact
      if nfacts == facts
        then askVariables vs nhs nfacts components
        else return nfacts
    askVariables [] _ facts _ = return facts

    askFacts no vs [] facts = return (no, facts)
    askFacts no vs (h : hs) facts = do
      answer <- askFact h hs facts
      -- close the loop if the answer is no
      if answer
        then do
          askFacts no vs hs (h : facts)
        else do
          return (h : no, facts)

prompt :: String -> IO String
prompt text = do
  putStr $ text ++ " "
  hFlush stdout
  getLine
