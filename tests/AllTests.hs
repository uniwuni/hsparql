{-# LANGUAGE OverloadedStrings #-}

module Main where

import Database.HSparql.ConnectionTest

import Control.Concurrent (forkIO)
import Network.HTTP.Types (status200)
import Network.Wai
import Network.Wai.Handler.Warp (run)
import Test.Framework

main :: IO ()
main =
  do _ <- forkIO $ startServer
     ropts <- interpretArgsOrExit []
     defaultMainWithOpts tests ropts
     where tests = Database.HSparql.ConnectionTest.testSuite

app :: Application
app req respond = respond $ response req
   where
     response req_ = case rawQueryString req_ of
                      "?query=PREFIX%20foaf%3A%20%3Chttp%3A%2F%2Fxmlns.com%2Ffoaf%2F0.1%2F%3E%20PREFIX%20dbpedia%3A%20%3Chttp%3A%2F%2Fdbpedia.org%2Fproperty%2F%3E%20PREFIX%20dbprop%3A%20%3Chttp%3A%2F%2Fdbpedia.org%2Fresource%2F%3E%20SELECT%20%3Fx1%20WHERE%20%7B%3Fx0%20dbpedia%3Agenre%20dbprop%3AWeb_browser%20.%20%3Fx0%20foaf%3Aname%20%3Fx1%20.%7D"
                          -> selectResponse
                      "?query=PREFIX%20dbprop%3A%20%3Chttp%3A%2F%2Fdbpedia.org%2Fproperty%2F%3E%20PREFIX%20dbpedia%3A%20%3Chttp%3A%2F%2Fdbpedia.org%2Fresource%2F%3E%20ASK%20%7B%20%3Fx0%20dbprop%3Agenre%20dbpedia%3AWeb_browser%20.%20%7D"
                          -> askResponse
                      "?query=PREFIX%20example%3A%20%3Chttp%3A%2F%2Fwww.example.com%2F%3E%20PREFIX%20foaf%3A%20%3Chttp%3A%2F%2Fxmlns.com%2Ffoaf%2F0.1%2F%3E%20PREFIX%20dbprop%3A%20%3Chttp%3A%2F%2Fdbpedia.org%2Fproperty%2F%3E%20PREFIX%20dbpedia%3A%20%3Chttp%3A%2F%2Fdbpedia.org%2Fresource%2F%3E%20CONSTRUCT%20%7B%20%3Fx0%20example%3AhasName%20%3Fx1%20.%20%7D%20WHERE%20%7B%3Fx0%20dbprop%3Agenre%20dbpedia%3AWeb_browser%20.%20%3Fx0%20foaf%3Aname%20%3Fx1%20.%7D"
                          -> constructResponse
                      "?query=PREFIX%20dbpedia%3A%20%3Chttp%3A%2F%2Fdbpedia.org%2Fresource%2F%3E%20DESCRIBE%20dbpedia%3AEdinburgh%20WHERE%20%7B%7D"
                          -> describeResponse
                      raw_req
                          -> error $ "Unexpected URI: \n\n" ++ show raw_req

     selectResponse = responseFile status200 [("Content-Type", "application/sparql-results+xml")] "tests/fixtures/sparql_select_response.xml" Nothing
     askResponse = responseFile status200 [("Content-Type", "text/plain")] "tests/fixtures/sparql_ask_response.text" Nothing
     constructResponse = responseFile status200 [("Content-Type", "text/turtle")] "tests/fixtures/sparql_construct_response.ttl" Nothing
     describeResponse = responseFile status200 [("Content-Type", "text/turtle")] "tests/fixtures/sparql_describe_response.ttl" Nothing


startServer :: IO ()
startServer = run 3000 app
