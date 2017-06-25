module Ex1 where

import Prelude hiding (Word)
import Control.Arrow ((&&&))
import Data.Char (toLower)
import Data.Ord (comparing)
import Data.Function (on)
import Data.List (groupBy, sort, sortBy)

type Word = String

anagrams :: Int -> [Word] -> String
anagrams n = concat . map showEntry .
             map mkEntry .
             groupBy ((==) `on` fst) .
             sortBy (comparing fst) .
             map (sort.map toLower &&& id) .
             filter ((==n) . length)
  where
    mkEntry lws = (head ls, ws)
      where
        (ls, ws) = unzip lws
    showEntry (l, ws) = l ++ concat ws
