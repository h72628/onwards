# Usage
Haskell stack is required (see
[here](https://docs.haskellstack.org/en/stable/README/)). Build with `stack
build` and run with `stack run`.

The output corresponds to the examples in Tables 1-4. For each table row, the output is structured as follows:
+ User utterance.
+ List of variable mappings. In some cases, the derivation may produce different
  but equivalent variables. This equivalence relation is stored in the map so
  that later we can unify all variables and remove duplicate suggestions.
+ List of facts from the user utterance and application context (if any).
+ List of questions that the user must answer to complete the derivation.
+ Resulting entities and templates.

To try other examples edit the file `examples/Example.hs` accordingly.
