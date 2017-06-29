module Database.HSparql.ConnectionTest ( testSuite ) where

import Test.Framework (testGroup, Test)
import Test.Framework.Providers.HUnit
import Test.HUnit

import qualified Data.Map as Map
import qualified Data.RDF as RDF

import Database.HSparql.Connection
import Database.HSparql.QueryGenerator

testSuite :: [Test.Framework.Test]
testSuite = [
    testGroup "Database.HSparql.Connection tests" [
        testCase "selectQuery" test_selectQuery
      , testCase "askQuery" test_askQuery
      , testCase "constructQuery" test_constructQuery
      , testCase "describeQuery" test_describeQuery
    ]
  ]

test_selectQuery :: IO ()
test_selectQuery =
  let expectedBVars = Just [ [ Bound $ RDF.lnode $ RDF.plainLL "Kazehakase" "en" ]
                           , [ Bound $ RDF.lnode $ RDF.plainLL "Netscape Browser" "en" ]
                           , [ Bound $ RDF.lnode $ RDF.plainLL "SlimBrowser" "en" ]
                           ]
  in do
    bvars <- selectQuery endPoint query
    assertEqual "bound variables" expectedBVars bvars

    where endPoint = "http://127.0.0.1:3000"
          query = do
              resource <- prefix "dbprop" (iriRef "http://dbpedia.org/resource/")
              dbpprop  <- prefix "dbpedia" (iriRef "http://dbpedia.org/property/")
              foaf     <- prefix "foaf" (iriRef "http://xmlns.com/foaf/0.1/")

              x    <- var
              name <- var

              _ <- triple x (dbpprop .:. "genre") (resource .:. "Web_browser")
              _ <- triple x (foaf .:. "name") name

              return SelectQuery { queryVars = [name] }

test_askQuery :: IO ()
test_askQuery = do
  bool <- askQuery endPoint query
  assertBool "invalid" bool

  where endPoint = "http://127.0.0.1:3000"
        query = do
            resource <- prefix "dbpedia" (iriRef "http://dbpedia.org/resource/")
            dbprop  <- prefix "dbprop" (iriRef "http://dbpedia.org/property/")

            x <- var
            ask <- askTriple x (dbprop .:. "genre") (resource .:. "Web_browser")

            return AskQuery { queryAsk = [ask] }

test_constructQuery :: IO ()
test_constructQuery =
  let expectedGraph :: RDF.RDF RDF.TList
      expectedGraph = RDF.mkRdf expectedTriples Nothing (RDF.PrefixMappings Map.empty)
      expectedTriples = [ RDF.Triple (RDF.unode "http://dbpedia.org/resource/Kazehakase")
                                     (RDF.unode "http://www.example.com/hasName")
                                     (RDF.lnode $ RDF.plainLL "Kazehakase" "en") ]
  in do
    graph <- constructQuery endPoint query :: IO (RDF.RDF RDF.TList)
    assertBool "RDF does not include the constructed triple" $ RDF.isIsomorphic expectedGraph graph

    where endPoint = "http://127.0.0.1:3000"
          query = do
              resource <- prefix "dbpedia" (iriRef "http://dbpedia.org/resource/")
              dbpprop  <- prefix "dbprop" (iriRef "http://dbpedia.org/property/")
              foaf     <- prefix "foaf" (iriRef "http://xmlns.com/foaf/0.1/")
              example  <- prefix "example" (iriRef "http://www.example.com/")

              x    <- var
              name <- var

              construct <- constructTriple x (example .:. "hasName") name
              _ <- triple x (dbpprop .:. "genre") (resource .:. "Web_browser")
              _ <- triple x (foaf .:. "name") name

              return ConstructQuery { queryConstructs = [construct] }

test_describeQuery :: IO ()
test_describeQuery =
  let expectedNode = RDF.unode "http://dbpedia.org/resource/Edinburgh"
  in do
    graph <- describeQuery endPoint query :: IO (RDF.RDF RDF.TList)
    assertBool "RDF does not include the required node" $ RDF.rdfContainsNode graph expectedNode

    where endPoint = "http://127.0.0.1:3000"
          query = do
              resource <- prefix "dbpedia" (iriRef "http://dbpedia.org/resource/")
              uri <- describeIRI (resource .:. "Edinburgh")
              return DescribeQuery { queryDescribe = uri }
