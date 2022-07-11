module Example (example) where

import Data
import qualified Data.Map as M
import LazyProofs

table1 :: [(String, [Fact])]
table1 = [sentence_4, sentence_0, sentence_3]
table2 :: [(String, [Fact])]
table2 = [sentence_4a, sentence_0a, sentence_3a]
table3 :: [(String, [Fact])]
table3 = [sentence_0a, sentence_1a, sentence_2a]
table4 :: [(String, [Fact])]
table4 = [sentence_0a1, sentence_0a2, sentence_0a3]
example :: [(String, [Fact])]
example=table1 ++ table2 ++ table3 ++ table4
-------------------------------
-- Sentences without context --
-------------------------------
sentence_4 :: (String, [Fact])
sentence_4 =
  ( "I want a page to create movies.",
    [ Create (Value "a_1"),
      Record (Value "movies"),
      What (Value "a_1") (Value "movies")
    ]
  )

sentence_0 :: (String, [Fact])
sentence_0 =
  ("I want to see a list.", [Show (Value "a0")])

sentence_3 :: (String, [Fact])
sentence_3 =
  ( "I want to see a list of movies filtered by title in page movies.",
    [ Show (Value "a_1"),
      Filter (Value "a_1"),
      Page (Value "movies"),
      Where (Value "a_1") (Value "movies"),
      Record (Value "movies"),
      What (Value "a_1") (Value "movies"),
      What (Value "a_1") (Value "title"),
      PartOf (Value "a_1") (Value "movies") (Value "title"),
      Field (Value "title")
    ]
  )

-------------------------------------------
-- Sentences with same context (table 2) --
-------------------------------------------

sentence_4a :: (String, [Fact])
sentence_4a =
  ( "I want a page to create movies.",
    [ Create (Value "a_1"),
      Record (Value "movies"),
      What (Value "a_1") (Value "movies"),
      Entity (Value "movies") (M.fromList [(Value "price", Value "currency"), (Value "stock", Value "integer"), (Value "title", Value "string")]),
      Entity (Value "customers") (M.fromList [(Value "email", Value "string")]),
      Entity (Value "renting") (M.fromList [(Value "email", Value "string"), (Value "title", Value "string")]),
      Location (Value "homepage"),
      Location (Value "admin"),
      Location (Value "best-selling")
    ]
  )

sentence_0a :: (String, [Fact])
sentence_0a =
  ( "I want to see a list.",
    [ Show (Value "a0"),
      Entity (Value "movies") (M.fromList [(Value "price", Value "currency"), (Value "stock", Value "integer"), (Value "title", Value "string")]),
      Entity (Value "customers") (M.fromList [(Value "email", Value "string")]),
      Entity (Value "renting") (M.fromList [(Value "email", Value "string"), (Value "title", Value "string")]),
      Location (Value "homepage"),
      Location (Value "admin"),
      Location (Value "best-selling")
    ]
  )

sentence_3a :: (String, [Fact])
sentence_3a =
  ( "I want to see a list of movies filtered by title.",
    [
      Show (Value "a_1"),
      Filter (Value "a_1"),
      Record (Value "movies"),
      What (Value "a_1") (Value "movies"),
      What (Value "a_1") (Value "title"),
      PartOf (Value "a_1") (Value "movies") (Value "title"),
      Field (Value "title"),
      Entity (Value "movies") (M.fromList [(Value "price", Value "currency"), (Value "stock", Value "integer"), (Value "title", Value "string")]),
      Entity (Value "customers") (M.fromList [(Value "email", Value "string")]),
      Entity (Value "renting") (M.fromList [(Value "email", Value "string"), (Value "title", Value "string")]),
      Location (Value "homepage"),
      Location (Value "admin"),
      Location (Value "best-selling")
    ]
  )

-------------------------------------------
-- Sentences with same context (table 3) --
-------------------------------------------


sentence_1a :: (String, [Fact])
sentence_1a =
  ( "I want to see a list of movies with price and title.",
    [ Show (Value "a_1"),
      Record (Value "movies"),
      What (Value "a_1") (Value "movies"),
      PartOf (Value "a_1") (Value "movies") (Value "title"),
      PartOf (Value "a_1") (Value "movies") (Value "price"),
      Field (Value "title"),
      Field (Value "price"),
      Entity (Value "movies") (M.fromList [(Value "price", Value "currency"), (Value "stock", Value "integer"), (Value "title", Value "string")]),
      Entity (Value "customers") (M.fromList [(Value "email", Value "string")]),
      Entity (Value "renting") (M.fromList [(Value "email", Value "string"), (Value "title", Value "string")]),
      Location (Value "homepage"),
      Location (Value "admin"),
      Location (Value "best-selling")
    ]
  )

sentence_2a :: (String, [Fact])
sentence_2a =
  ( "I want to see a list of movies with price and title in page movies.",
    [ Show (Value "a_1"),
      Record (Value "movies"),
      Page (Value "movies"),
      Where (Value "a_1") (Value "movies"),
      What (Value "a_1") (Value "movies"),
      PartOf (Value "a_1") (Value "movies") (Value "title"),
      PartOf (Value "a_1") (Value "movies") (Value "price"),
      Field (Value "title"),
      Field (Value "price"),
      Entity (Value "movies") (M.fromList [(Value "price", Value "currency"), (Value "stock", Value "integer"), (Value "title", Value "string")]),
      Entity (Value "customers") (M.fromList [(Value "email", Value "string")]),
      Entity (Value "renting") (M.fromList [(Value "email", Value "string"), (Value "title", Value "string")]),
      Location (Value "homepage"),
      Location (Value "admin"),
      Location (Value "best-selling")
    ]
  )

----------------------------------------------
-- Sentence with variable context (table 4) --
----------------------------------------------

sentence_0a1 :: (String, [Fact])
sentence_0a1 =
  ( "I want to see a list.",
    [ Show (Value "a_0"),
      Entity (Value "movies") (M.fromList [(Value "price", Value "currency"), (Value "stock", Value "integer"), (Value "title", Value "string")]),
      Location (Value "homepage")
    ]
  )

sentence_0a2 :: (String, [Fact])
sentence_0a2 =
  ( "I want to see a list.",
    [ Show (Value "a_0"),
      Entity (Value "movies") (M.fromList [(Value "price", Value "currency"), (Value "stock", Value "integer"), (Value "title", Value "string")]),
      Location (Value "homepage"),
      Location (Value "best-selling")
    ]
  )

sentence_0a3 :: (String, [Fact])
sentence_0a3 =
  ( "I want to see a list.",
    [ Show (Value "a_0"),
      Entity (Value "movies") (M.fromList [(Value "price", Value "currency"), (Value "stock", Value "integer"), (Value "title", Value "string")]),
      Entity (Value "customers") (M.fromList [(Value "email", Value "string")]),
      Entity (Value "renting") (M.fromList [(Value "email", Value "string"), (Value "title", Value "string")]),
      Location (Value "homepage"),
      Location (Value "admin"),
      Location (Value "best-selling"),
      Location (Value "movie-page"),
      Location (Value "profile-page")
    ]
  )
