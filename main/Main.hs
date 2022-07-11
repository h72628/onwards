module Main where

import Example ( example )
import qualified Data.Map as M
import Questions (infer)
import Text.Show.Pretty (pPrint)
import LazyProofs(list_union)
main :: IO [()]
main =
  mapM (\(sentence, facts) -> (do
                                pPrint sentence
                                ((_, _, assoc), sequents) <- infer (0, M.empty, M.empty) facts
                                pPrint assoc
                                let questions = list_union $ map (\(_, q, _) -> q) sequents
                                let results = map (\(_, _, r) -> r) sequents
                                putStr "Facts: "
                                pPrint facts
                                putStr "Questions: "
                                pPrint questions
                                putStr "Results: "
                                pPrint results
                                pPrint "-----------")) example
