module Database.HSparql.Connection
    ( EndPoint
    , BindingValue(..)
    , selectQuery
    , constructQuery
    )
where

import Control.Monad
import Data.Maybe
import qualified Network.HTTP as HTTP
import Text.XML.Light

import Database.HSparql.QueryGenerator

import Text.RDF.RDF4H.XmlParser
import Data.RDF.TriplesGraph
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

-- |Connect to remote 'EndPoint' and find all possible bindings for the
--  'Variable's in the 'SelectQuery' action.
selectQuery :: EndPoint -> Query SelectQuery -> IO (Maybe [[BindingValue]])
selectQuery ep q = do
    let uri      = ep ++ "?" ++ HTTP.urlEncodeVars [("query", createSelectQuery q)]
        request  = HTTP.replaceHeader HTTP.HdrUserAgent "hsparql-client" (HTTP.getRequest uri)
    response <- HTTP.simpleHTTP request >>= HTTP.getResponseBody
    return $ structureContent response

-- |Connect to remote 'EndPoint' and construct 'TriplesGraph' from given
--  'ConstructQuery' action. /Provisional implementation/.
constructQuery :: EndPoint -> Query ConstructQuery -> IO TriplesGraph
constructQuery ep q = do
    let uri      = ep ++ "?" ++ HTTP.urlEncodeVars [("query", createConstructQuery q)]
        h1 = HTTP.mkHeader HTTP.HdrUserAgent "hsparql-client"
        h2 = HTTP.mkHeader HTTP.HdrAccept "application/rdf+xml"
        request = HTTP.Request { HTTP.rqURI = fromJust $ parseURI uri
                          , HTTP.rqHeaders = [h1,h2]
                          , HTTP.rqMethod = HTTP.GET
                          , HTTP.rqBody = ""
                          }
    response <- HTTP.simpleHTTP request >>= HTTP.getResponseBody
    putStrLn response
    let rdfGraph = parseXmlRDF Nothing Nothing (B.pack response)
    case rdfGraph of
     Left e -> error $ show e
     Right graph -> return graph
    