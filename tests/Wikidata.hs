{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Wikidata where

import Control.Monad (forM_)
import Data.Maybe
import Database.HSparql.Connection
import Database.HSparql.QueryGenerator

wikidataTest :: IO ()
wikidataTest = do
  (Just s) <- selectQuery "https://query.wikidata.org/sparql" wdtQuery
  forM_ s print

wdtQuery :: Query SelectQuery
wdtQuery = do
  s <- var
  p <- var
  o <- var

  triple s p o

  limit 10

  selectVars [s, p, o]
