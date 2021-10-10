{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (forkIO)
import Control.Monad (join)
import DBPedia () -- to ensure the DBPedia.hs file compiles
import Database.HSparql.ConnectionTest
import Database.HSparql.QueryGeneratorTest
import Network.HTTP.Types (status200)
import Network.Wai
import Network.Wai.Handler.Warp (run)
import Test.Framework
import Wikidata () -- to ensure the Wikidata.hs file compiles

main :: IO ()
main =
  do
    _ <- forkIO $ startServer
    ropts <- interpretArgsOrExit []
    defaultMainWithOpts tests ropts
  where
    tests =
      Database.HSparql.ConnectionTest.testSuite
        ++ Database.HSparql.QueryGeneratorTest.testSuite

app :: Application
app req respond = respond $ response req
  where
    response req_ = case (join $ lookup "query" $ queryString req_) of
      Just "PREFIX dbprop: <http://dbpedia.org/resource/> PREFIX dbpedia: <http://dbpedia.org/property/> PREFIX foaf: <http://xmlns.com/foaf/0.1/> SELECT ?x1 WHERE { ?x0 dbpedia:genre dbprop:Web_browser . ?x0 foaf:name ?x1 . }" ->
        selectResponse
      Just "PREFIX foaf: <http://xmlns.com/foaf/0.1/> PREFIX dct: <http://purl.org/dc/elements/1.1/> SELECT ?x1 ?x2 WHERE { ?x0 foaf:name \"Bob\" . << ?x0 foaf:age ?x1 >> dct:source ?x2 . }" ->
        selectReifiedTripleResponse
      Just "PREFIX dbpedia: <http://dbpedia.org/resource/> PREFIX dbprop: <http://dbpedia.org/property/> ASK { ?x0 dbprop:genre dbpedia:Web_browser . }" ->
        askResponse
      Just "PREFIX dbpedia: <http://dbpedia.org/resource/> PREFIX dbprop: <http://dbpedia.org/property/> PREFIX foaf: <http://xmlns.com/foaf/0.1/> PREFIX example: <http://www.example.com/> CONSTRUCT { ?x0 example:hasName ?x1 . } WHERE { ?x0 dbprop:genre dbpedia:Web_browser . ?x0 foaf:name ?x1 . }" ->
        constructResponse
      Just "PREFIX dbpedia: <http://dbpedia.org/resource/> DESCRIBE dbpedia:Edinburgh WHERE {  }" ->
        describeResponse
      raw_req ->
        error $ "Unexpected URI: \n\n" ++ show raw_req

    selectResponse = responseFile status200 [("Content-Type", "application/sparql-results+xml")] "tests/fixtures/sparql_select_response.xml" Nothing
    selectReifiedTripleResponse = responseFile status200 [("Content-Type", "application/sparql-results+xml")] "tests/fixtures/sparql_select_reified_triple_response.xml" Nothing
    askResponse = responseFile status200 [("Content-Type", "text/plain")] "tests/fixtures/sparql_ask_response.text" Nothing
    constructResponse = responseFile status200 [("Content-Type", "text/turtle")] "tests/fixtures/sparql_construct_response.ttl" Nothing
    describeResponse = responseFile status200 [("Content-Type", "text/turtle")] "tests/fixtures/sparql_describe_response.ttl" Nothing

startServer :: IO ()
startServer = run 3000 app
