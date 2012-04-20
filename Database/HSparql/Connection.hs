module Database.HSparql.Connection
    ( Database.HSparql.Connection.EndPoint
    , BindingValue(..)
    , selectQuery
    , constructQuery
    , askQuery
    , describeQuery
    )
where

import Control.Monad
import Data.Maybe
import Network.HTTP
import Text.XML.Light
import Database.HSparql.QueryGenerator
import Data.RDF.MGraph
import Text.RDF.RDF4H.TurtleParser
import Data.RDF
import qualified Data.ByteString.Lazy.Char8 as B

import Network.URI hiding (URI)

-- |URI of the SPARQL endpoint.
type EndPoint = String

-- |Local representations of incoming XML results.
data BindingValue = URI String                 -- ^Absolute reference to remote resource.
                  | Literal String             -- ^Simple literal string.
                  | TypedLiteral String String -- ^Literal element with type resource
                  | LangLiteral String String  -- ^Literal element with language resource
                  | Unbound                    -- ^Unbound result value
  deriving (Show, Eq)

-- |Base 'QName' for results with a SPARQL-result URI specified.
sparqlResult :: String -> QName
sparqlResult s = (unqual s) { qURI = Just "http://www.w3.org/2005/sparql-results#" }

-- |Transform the 'String' result from the HTTP request into a two-dimensional
--  table storing the bindings for each variable in each row.
structureContent :: String -> Maybe [[BindingValue]]
structureContent s =
    do e <- doc
       return $ map (projectResult $ vars e) $ findElements (sparqlResult "result") e
    where doc :: Maybe Element
          doc = parseXMLDoc s

          vars :: Element -> [String]
          vars = mapMaybe (findAttr $ unqual "name") . findElements (sparqlResult "variable")

          projectResult :: [String] -> Element -> [BindingValue]
          projectResult vs e = map pVar vs
             where pVar v   = maybe Unbound (value . head . elChildren) $ filterElement (pred v) e
                   pred v e = isJust $ do a <- findAttr (unqual "name") e
                                          guard $ a == v

          value :: Element -> BindingValue
          value e =
            case qName (elName e) of
              "uri"     -> URI (strContent e)
              "literal" -> case findAttr (unqual "datatype") e of
                             Just dt -> TypedLiteral (strContent e) dt
                             Nothing -> case findAttr langAttr e of
                                          Just lang -> LangLiteral (strContent e) lang
                                          Nothing   -> Literal (strContent e)
              _         -> Unbound

          langAttr :: QName
          langAttr = blank_name { qName = "lang", qPrefix = Just "xml" }

-- |Parses the response from a SPARQL ASK query. Either "true" or "false" is expected
parseAsk :: String -> Bool
parseAsk s
  | s == "true" = True
  | s == "false" = False
  | otherwise = error $ "Unexpected Ask response: " ++ s

-- |Connect to remote 'EndPoint' and find all possible bindings for the
--  'Variable's in the 'SelectQuery' action.
selectQuery :: Database.HSparql.Connection.EndPoint -> Query SelectQuery -> IO (Maybe [[BindingValue]])
selectQuery ep q = do
    let uri      = ep ++ "?" ++ urlEncodeVars [("query", createSelectQuery q)]
        request  = replaceHeader HdrUserAgent "hsparql-client" (getRequest uri)
    response <- simpleHTTP request >>= getResponseBody
    return $ structureContent response

-- |Connect to remote 'EndPoint' and find all possible bindings for the
--  'Variable's in the 'SelectQuery' action.
askQuery :: Database.HSparql.Connection.EndPoint -> Query AskQuery -> IO Bool
askQuery ep q = do
    let uri      = ep ++ "?" ++ urlEncodeVars [("query", createAskQuery q)]
        request  = replaceHeader HdrUserAgent "hsparql-client" (getRequest uri)
    response <- simpleHTTP request >>= getResponseBody
    return $ parseAsk response


-- |Connect to remote 'EndPoint' and construct 'TriplesGraph' from given
--  'ConstructQuery' action. /Provisional implementation/.
constructQuery :: Database.HSparql.Connection.EndPoint -> Query ConstructQuery -> IO MGraph
constructQuery ep q = do
    let uri      = ep ++ "?" ++ urlEncodeVars [("query", createConstructQuery q)]
    rdfGraph <- httpCallForRdf uri
    case rdfGraph of
     Left e -> error $ show e
     Right graph -> return graph
   

-- |Connect to remote 'EndPoint' and construct 'TriplesGraph' from given
--  'ConstructQuery' action. /Provisional implementation/.
describeQuery :: Database.HSparql.Connection.EndPoint -> Query DescribeQuery -> IO MGraph
describeQuery ep q = do
    let uri      = ep ++ "?" ++ urlEncodeVars [("query", createDescribeQuery q)]
    rdfGraph <- httpCallForRdf uri
    case rdfGraph of
     Left e -> error $ show e
     Right graph -> return graph
    
-- |Takes a generated uri and makes simple HTTP request,
-- asking for RDF N3 serialization. Returns either 'ParseFailure' or 'RDF'
httpCallForRdf :: RDF rdf => String -> IO (Either ParseFailure rdf)
httpCallForRdf uri = do
 let h1 = mkHeader HdrUserAgent "hsparql-client"
     h2 = mkHeader HdrAccept "text/rdf+n3"
     request = Request { rqURI = fromJust $ parseURI uri
                          , rqHeaders = [h1,h2]
                          , rqMethod = GET
                          , rqBody = ""
                          }
 response <- simpleHTTP request >>= getResponseBody
 return $ parseString (TurtleParser Nothing Nothing) (B.pack response)