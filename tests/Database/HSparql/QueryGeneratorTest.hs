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
normalizeWhitespace = T.strip
                      . (T.replace "  " " ")
                      . (T.replace "  " " ")
                      . (T.replace "  " " ")
                      . (T.replace "\n" " ")

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

  -- https://github.com/robstewart57/hsparql/pull/31
  , ( [s|
PREFIX : <http://example1.com/>
PREFIX ex: <http://example2.com/>
SELECT ?x0 WHERE {
  ?x0 :property1 :X .
  FILTER ((?x1>(10))&&(?x1<(20))) .
  {
    SELECT ((AVG(?x0_0)) AS ?x1)
    WHERE {
      ?x0 :property2 [ex:property3 ?x0_0] .
    }
  }
}
|]
    , createQuery $ do
      p1 <- prefix "" (iriRef "http://example1.com/")
      s <- var
      v <- var
      triple_ s (p1 .:. "property1") (p1 .:. "X")
      filterExpr_ $ (v .>. (10::Integer)) .&&. (v .<. (20::Integer))
      subQuery_ $ do
        p2 <- prefix "ex" (iriRef "http://example2.com/")
        v' <- var
        triple_ s (p1 .:. "property2") [mkPredicateObject (p2 .:. "property3") v']
        select [avg v' `as` v]
      selectVars [s]
    )

  -- https://www.w3.org/TR/sparql11-query/#OptionalMatching
  , ( [s|
PREFIX foaf: <http://xmlns.com/foaf/0.1/>
SELECT ?x1 ?x2
WHERE {
  ?x0 foaf:name ?x1 .
  OPTIONAL { ?x0 foaf:mbox ?x2 . }
}
|]
    , createQuery $ do
        foaf     <- prefix "foaf" (iriRef "http://xmlns.com/foaf/0.1/")

        x <- var
        name <- var
        mbox <- var

        triple_ x (foaf .:. "name") name

        optional_ $ do
          triple_ x (foaf .:. "mbox") mbox

        selectVars [name, mbox]
    )

  -- https://www.w3.org/TR/sparql11-query/#alternatives
  , ( [s|
PREFIX dc10: <http://purl.org/dc/elements/1.0/>
PREFIX dc11: <http://purl.org/dc/elements/1.1/>

SELECT ?x1
WHERE { { ?x0 dc10:title ?x1 . } UNION { ?x0 dc11:title ?x1 . } }
|]
    , createQuery $ do
        dc10     <- prefix "dc10" (iriRef "http://purl.org/dc/elements/1.0/")
        dc11     <- prefix "dc11" (iriRef "http://purl.org/dc/elements/1.1/")

        book <- var
        title <- var

        let tt10 = triple_ book (dc10 .:. "title") title
            tt11 = triple_ book (dc11 .:. "title") title
          in union_ tt10 tt11

        selectVars [title]
    )

  -- https://www.w3.org/TR/sparql11-query/#neg-notexists
  , ( [s|
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX foaf: <http://xmlns.com/foaf/0.1/>

SELECT ?x0
WHERE
{
  ?x0 rdf:type foaf:Person .
  FILTER NOT EXISTS { ?x0 foaf:name ?x1 . }
}
|]
    , createQuery $ do
        rdf  <- prefix "rdf" (iriRef "http://www.w3.org/1999/02/22-rdf-syntax-ns#")
        foaf <- prefix "foaf" (iriRef "http://xmlns.com/foaf/0.1/")

        person <- var
        name <- var

        triple_ person (rdf .:. "type") (foaf .:. "Person")

        filterNotExists_ $ do
          triple_ person (foaf .:. "name") name

        selectVars [person]
    )

  ]

testSuite :: [Test.Framework.Test]
testSuite = [
    testGroup "Database.HSparql.QueryGenerator tests" $ (`map` queryTexts) $
      \(expected, actual) -> testCase (T.unpack expected) $ do
        assertEqual "" (normalizeWhitespace expected) (normalizeWhitespace actual)
  ]
