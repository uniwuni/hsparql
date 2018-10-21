{-# LANGUAGE QuasiQuotes #-}

module Database.HSparql.QueryGeneratorTest ( testSuite ) where

import Test.Framework (testGroup, Test)
import Test.Framework.Providers.HUnit
import Test.HUnit

import Data.Text (Text)
import qualified Data.Text as T
import Data.String.QQ (s)

import Database.HSparql.QueryGenerator

class CreateQuery a where
  createQuery :: a -> Text

instance CreateQuery (Query SelectQuery) where
  createQuery = T.pack . createSelectQuery

instance CreateQuery (Query ConstructQuery) where
  createQuery = T.pack . createConstructQuery

instance CreateQuery (Query AskQuery) where
  createQuery = T.pack . createAskQuery

instance CreateQuery (Query UpdateQuery) where
  createQuery = T.pack . createUpdateQuery

instance CreateQuery (Query DescribeQuery) where
  createQuery = T.pack . createDescribeQuery

normalizeWhitespace :: Text -> Text
normalizeWhitespace = T.strip . (T.replace "   " " ") . (T.replace "\n" " ")

queryTexts :: [(Text, Text)]
queryTexts =
  [ ( [s|
PREFIX dbpedia: <http://dbpedia.org/resource/>
PREFIX dbprop: <http://dbpedia.org/property/>
PREFIX foaf: <http://xmlns.com/foaf/0.1/>
SELECT ?x1 ?x2
WHERE {
  ?x0 dbprop:genre dbpedia:Web_browser .
  ?x0 foaf:name ?x1 .
  ?x0 foaf:page ?x2 .
}
|]
    , createQuery $ do
        resource <- prefix "dbpedia" (iriRef "http://dbpedia.org/resource/")
        dbpprop  <- prefix "dbprop" (iriRef "http://dbpedia.org/property/")
        foaf     <- prefix "foaf" (iriRef "http://xmlns.com/foaf/0.1/")

        x    <- var
        name <- var
        page <- var

        triple_ x (dbpprop .:. "genre") (resource .:. "Web_browser")
        triple_ x (foaf .:. "name") name
        triple_ x (foaf .:. "page") page

        selectVars [name, page]
    )

  , ( [s|
PREFIX dbpedia: <http://dbpedia.org/resource/>
PREFIX dbprop: <http://dbpedia.org/property/>
PREFIX foaf: <http://xmlns.com/foaf/0.1/>
PREFIX example: <http://www.example.com/>
CONSTRUCT {
  ?x0 example:hasName ?x1 .
}
WHERE {
  ?x0 dbprop:genre dbpedia:Web_browser .
  ?x0 foaf:name ?x1 .
  ?x0 foaf:page ?x2 .
}
|]
    , createQuery $ do
        resource <- prefix "dbpedia" (iriRef "http://dbpedia.org/resource/")
        dbpprop  <- prefix "dbprop" (iriRef "http://dbpedia.org/property/")
        foaf     <- prefix "foaf" (iriRef "http://xmlns.com/foaf/0.1/")
        example  <- prefix "example" (iriRef "http://www.example.com/")

        x    <- var
        name <- var
        page <- var

        construct <- constructTriple x (example .:. "hasName") name

        triple_ x (dbpprop .:. "genre") (resource .:. "Web_browser")
        triple_ x (foaf .:. "name") name
        triple_ x (foaf .:. "page") page

        return ConstructQuery { queryConstructs = [construct] }
    )

  , ( [s|
PREFIX dbpedia: <http://dbpedia.org/resource/> DESCRIBE dbpedia:Edinburgh WHERE {  }
|]
    , createQuery $ do
        resource <- prefix "dbpedia" (iriRef "http://dbpedia.org/resource/")
        uri <- describeIRI (resource .:. "Edinburgh")
        return DescribeQuery { queryDescribe = uri }
    )

  , ( [s|
PREFIX dbpedia: <http://dbpedia.org/resource/>
PREFIX dbprop: <http://dbpedia.org/property/>
ASK {
  ?x0 dbprop:genre dbpedia:Web_browser .
}
|]
    , createQuery $ do
        resource <- prefix "dbpedia" (iriRef "http://dbpedia.org/resource/")
        dbprop  <- prefix "dbprop" (iriRef "http://dbpedia.org/property/")

        x <- var
        ask <- askTriple x (dbprop .:. "genre") (resource .:. "Web_browser")

        return AskQuery { queryAsk = [ask] }
    )

  ]

testSuite :: [Test.Framework.Test]
testSuite = [
    testGroup "Database.HSparql.QueryGenerator tests" $ (`map` queryTexts) $
      \(expected, actual) -> testCase (T.unpack expected) $ do
        assertEqual "" (normalizeWhitespace expected) (normalizeWhitespace actual)
  ]
