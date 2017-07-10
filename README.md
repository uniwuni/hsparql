
[![Available on Hackage][badge-hackage]][hackage]
[![License BSD3][badge-license]][license]
[![Build Status][badge-travis]][travis]

[badge-travis]: https://travis-ci.org/robstewart57/hsparql.png?branch=master
[travis]: https://travis-ci.org/robstewart57/hsparql
[badge-hackage]: https://img.shields.io/hackage/v/hsparql.svg
[hackage]: http://hackage.haskell.org/package/hsparql
[badge-license]: https://img.shields.io/badge/license-BSD3-green.svg?dummy
[license]: https://github.com/robstewart57/hsparql/blob/master/LICENSE

## Introduction

hsparql includes a DSL to easily create queries, as well as methods to
submit those queries to a SPARQL server, returning the results as
simple Haskell data structures.

### Select Queries

Take the following SPARQL query:

    PREFIX dbpedia: <http://dbpedia.org/resource/>
    PREFIX dbprop: <http://dbpedia.org/property/>
    PREFIX foaf: <http://xmlns.com/foaf/0.1/>
    SELECT ?name ?page
    WHERE {
    ?x dbprop:genre dbpedia:Web_browser
    ?x foaf:name ?name
    ?x foaf:page ?page
    }



Can be generated using the following Haskell code:

    simpleSelect :: Query SelectQuery
    simpleSelect = do
        resource <- prefix "dbpedia" (iriRef "http://dbpedia.org/resource/")
        dbpprop  <- prefix "dbprop" (iriRef "http://dbpedia.org/property/")
        foaf     <- prefix "foaf" (iriRef "http://xmlns.com/foaf/0.1/")

        x    <- var
        name <- var
        page <- var

        triple x (dbpprop .:. "genre") (resource .:. "Web_browser")

        triple x (foaf .:. "name") name
        triple x (foaf .:. "page") page

        return SelectQuery { queryVars = [name, page] }


### Construct Queries

Take the following SPARQL query:

        PREFIX dbpedia: <http://dbpedia.org/resource/>
        PREFIX dbprop: <http://dbpedia.org/property/>
        PREFIX foaf: <http://xmlns.com/foaf/0.1/>
        PREFIX example: <http://www.example.com/>
        CONSTRUCT {
        ?x example:hasName ?name
        }
        WHERE {
        ?x dbprop:genre resource:Web_browser
        ?x foaf:name ?name
        ?x foaf:page ?page
        }

Can be generated using the following Haskell code:

        simpleConstruct :: Query ConstructQuery
        simpleConstruct = do
            resource <- prefix "dbpedia" (iriRef "http://dbpedia.org/resource/")
            dbpprop  <- prefix "dbprop" (iriRef "http://dbpedia.org/property/")
            foaf     <- prefix "foaf" (iriRef "http://xmlns.com/foaf/0.1/")
            example  <- prefix "example" (iriRef "http://www.example.com/")

            x    <- var
            name <- var
            page <- var

            construct <- constructTriple x (example .:. "hasName") name

            triple x (dbpprop .:. "genre") (resource .:. "Web_browser")
            triple x (foaf .:. "name") name
            triple x (foaf .:. "page") page

            return ConstructQuery { queryConstructs = [construct] }


### Describe Queries

Take the following SPARQL query:

    DESCRIBE <http://dbpedia.org/resource/Edinburgh>

Can be generated using the following Haskell code:

    simpleDescribe :: Query DescribeQuery
    simpleDescribe = do
        resource <- prefix "dbpedia" (iriRef "http://dbpedia.org/resource/")
        uri <- describeIRI (resource .:. "Edinburgh")
        return DescribeQuery { queryDescribe = uri }

### Ask Queries

Take the following SPARQL query:

    PREFIX dbprop: <http://dbpedia.org/property/>
    ASK { ?x dbprop:genre <http://dbpedia.org/resource/Web_browser> }

Can be generated using the following Haskell code:

    simpleAsk :: Query AskQuery
    simpleAsk = do
        resource <- prefix "dbpedia" (iriRef "http://dbpedia.org/resource/")
        dbprop  <- prefix "dbprop" (iriRef "http://dbpedia.org/property/")

        x <- var
        ask <- askTriple x (dbprop .:. "genre") (resource .:. "Web_browser")

        return AskQuery { queryAsk = [ask] }

## Output Types

### Select Queries

SELECT queries generate a set of sparql query solutions. See:
http://www.w3.org/TR/rdf-sparql-XMLres/

    selectExample :: IO ()
    selectExample = do
      (Just s) <- selectQuery "http://dbpedia.org/sparql" simpleSelect
      putStrLn . take 500 . show $ s


Here's the respective type:

    selectQuery :: EndPoint -> Query SelectQuery -> IO (Maybe [[BindingValue]])


### Construct Queries

CONSTRUCT queries generate RDF, which is serialized in N3 in this
package. See:
http://www.w3.org/TR/rdf-primer/#rdfxml

    constructExample :: IO ()
    constructExample = do
      rdfGraph <- constructQuery "http://dbpedia.org/sparql" simpleConstruct
      mapM_ print (triplesOf rdfGraph)

Here's the respective type:

    constructQuery :: EndPoint -> Query ConstructQuery -> IO MGraph

### Describe Queries

DESCRIBE queries generate RDF, which is serialized in N3 in this
package. See:
http://www.w3.org/TR/rdf-sparql-query/#describe

    describeExample :: IO ()
    describeExample = do
      rdfGraph <- describeQuery "http://dbpedia.org/sparql" simpleDescribe
      mapM_ print (triplesOf rdfGraph

Here's the respective type:

    describeQuery :: EndPoint -> Query DescribeQuery -> IO MGraph

### Ask Queries

ASK queries inspects whether or not a triple exists. RDF is an
open-world assumption. See:
http://www.w3.org/TR/rdf-sparql-query/#ask

    askExample :: IO ()
    askExample = do
      res <- askQuery "http://dbpedia.org/sparql" simpleAsk
      putStrLn $ "result: " ++ (show (res::Bool))

Here's the respective type:

    askQuery :: EndPoint -> Query AskQuery -> IO Bool


## TODOs

- Opt for a unified Type representation
This hsparql package and the RDF4H [1] package use similar, but not
identical, types for triples, namespaces, prefixes and so on. Ideally,
one type representation for such concepts should be adopted for both packages.

- Develop a unified semantic web toolkit for Haskell
Combining the RDF4H and hsparql packages seems like a sensible goal to
achieve, to provide a semantic web toolkit similar to Jena [2] for Java.


[1] - https://github.com/amccausl/RDF4H

[2] - http://incubator.apache.org/jena/
