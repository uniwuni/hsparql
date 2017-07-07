{-# LANGUAGE ExistentialQuantification #-}

-- |The query generator DSL for SPARQL, used when connecting to remote
--  endpoints.
module Database.HSparql.QueryGenerator
  ( -- * Creating Queries
    createSelectQuery
  , createConstructQuery
  , createAskQuery
  , createUpdateQuery
  , createDescribeQuery
  -- * Query Actions
  , prefix
  , var
  , Database.HSparql.QueryGenerator.triple
  , mkPredicateObject
  , constructTriple
  , askTriple
  , updateTriple
  , describeIRI
  , optional
  , union
  , filterExpr
  , bind

  -- ** Duplicate handling
  , distinct
  , reduced

  -- ** Limit handling
  , limit

  -- ** Order handling
  , orderNext
  , orderNextAsc
  , orderNextDesc

  -- ** Auxiliary
  , (.:.)
  , iriRef

  -- * Term Manipulation

  -- ** Operations
  , (.+.), (.-.), (.*.), (./.), (.&&.), (.||.)

  -- ** Relations
  , (.==.), (.!=.), (.<.), (.>.), (.<=.), (.>=.)

  -- ** Negation
  , notExpr

  -- ** Builtin Functions
  , str
  , lang
  , langMatches
  , datatype
  , bound
  , sameTerm
  , isIRI
  , isURI
  , isBlank
  , isLiteral
  , regex, regexOpts

  -- * Printing Queries
  , qshow

  -- * Types
  , Query
  , Variable
  , VarOrNode(..)
  , BlankNodePattern
  , Pattern
  , SelectQuery(..)
  , ConstructQuery(..)
  , AskQuery(..)
  , UpdateQuery(..)
  , DescribeQuery(..)

  -- * Classes
  , TermLike (..)
  , SubjectTermLike
  , PredicateTermLike
  , ObjectTermLike
  )
where

import Control.Monad.State
import Data.List (intercalate)
import qualified Data.Text as T
import qualified Data.RDF as RDF

-- State monads

-- |The 'State' monad applied to 'QueryData'.
type Query a = State QueryData a

-- |Execute a 'Query' action, starting with the empty 'queryData', then process
-- the resulting 'QueryData'.
execQuery :: Query a -> (QueryData -> b) -> b
execQuery q f = f $ execState q queryData

-- |Execute a 'Select Query' action, returning the 'String' representation of the query.
createSelectQuery :: Query SelectQuery -> String
createSelectQuery q = execQuery specifyVars qshow
  where specifyVars :: Query ()
        specifyVars = do
          query <- q
          modify $ \s -> s { vars = queryVars query , queryType = SelectType }

-- |Execute a 'Construct Query' action, returning the 'String' representation of the query.
createConstructQuery :: Query ConstructQuery -> String
createConstructQuery q = execQuery specifyType qshow
  where specifyType :: Query ()
        specifyType = do
          query <- q
          modify $ \s -> s { constructTriples = queryConstructs query, queryType = ConstructType }

-- |Execute a 'Ask Query' action, returning the 'String' representation of the query.
createAskQuery :: Query AskQuery -> String
createAskQuery q = execQuery specifyType qshow
  where specifyType :: Query ()
        specifyType = do
          query <- q
          modify $ \s -> s { askTriples = queryAsk query, queryType = AskType }

-- |Execute a 'Update Query' action, returning the 'String' representation of the query.
createUpdateQuery :: Query UpdateQuery -> String
createUpdateQuery q = execQuery specifyType qshow
  where specifyType :: Query ()
        specifyType = do
          query <- q
          modify $ \s -> s { updateTriples = queryUpdate query, queryType = UpdateType }

-- |Execute a 'Describe Query' action, returning the 'String' representation of the query.
createDescribeQuery :: Query DescribeQuery -> String
createDescribeQuery q = execQuery specifyType qshow
  where specifyType :: Query ()
        specifyType = do
          query <- q
          modify $ \s -> s { describeURI = Just (queryDescribe query), queryType = DescribeType }

-- Manipulate data within monad

-- |Add a prefix to the query, given an IRI reference, and return it.
prefix :: T.Text -> IRIRef -> Query Prefix
prefix pre (AbsoluteIRI node) = do
  let p = Prefix pre node
  modify $ \s -> s { prefixes = p : prefixes s }
  return p
prefix _ _ = error "prefix requires an absolute IRI"

-- |Create and return a variable to the query, usable in later expressions.
var :: Query Variable
var = do n <- gets varsIdx
         modify $ \s -> s { varsIdx = n + 1 }
         return $ Variable n

-- |Restrict the query to only results for which values match constants in this
--  triple, or for which the variables can be bound.
triple :: (SubjectTermLike a, PredicateTermLike b, ObjectTermLike c) => a -> b -> c -> Query Pattern
triple a b c = do
  let t = QTriple (varOrTerm a) (varOrTerm b) (varOrTerm c)
  modify $ \s -> s { pattern = appendPattern t (pattern s) }
  return t

constructTriple :: (SubjectTermLike a, PredicateTermLike b, ObjectTermLike c) => a -> b -> c -> Query Pattern
constructTriple a b c = do
  let t = QTriple (varOrTerm a) (varOrTerm b) (varOrTerm c)
  modify $ \s -> s { constructTriples = appendTriple t (constructTriples s) }
  return t

askTriple :: (SubjectTermLike a, PredicateTermLike b, ObjectTermLike c) => a -> b -> c -> Query Pattern
askTriple a b c = do
  let t = QTriple (varOrTerm a) (varOrTerm b) (varOrTerm c)
  modify $ \s -> s { askTriples = appendTriple t (askTriples s) }
  return t

updateTriple :: (SubjectTermLike a, PredicateTermLike b, ObjectTermLike c) => a -> b -> c -> Query Pattern
updateTriple a b c = do
  let t = QTriple (varOrTerm a) (varOrTerm b) (varOrTerm c) -- TODO: should only allow terms
  modify $ \s -> s { updateTriples = appendTriple t (updateTriples s) }
  return t

describeIRI :: IRIRef -> Query IRIRef
describeIRI newIri = do
  modify $ \s -> s { describeURI = Just newIri }
  return newIri


-- |Add optional constraints on matches. Variable bindings within the optional
--  action are lost, so variables must always be defined prior to opening the
--  optional block.
optional :: Query a -> Query Pattern
optional q = do
  -- Determine the patterns by executing the action on a blank QueryData, and
  -- then pulling the patterns out from there.
  let option  = execQuery q $ OptionalGraphPattern . pattern
  modify $ \s -> s { pattern = appendPattern option (pattern s) }
  return option

-- |Add a union structure to the query pattern. As with 'optional' blocks,
--  variables must be defined prior to the opening of any block.
union :: Query a -> Query b -> Query Pattern
union q1 q2 = do
  let p1    = execQuery q1 pattern
      p2    = execQuery q2 pattern
      union' = UnionGraphPattern p1 p2
  modify $ \s -> s { pattern = appendPattern union' (pattern s) }
  return union'

-- |Restrict results to only those for which the given expression is true.
filterExpr :: (TermLike a) => a -> Query Pattern
filterExpr e = do
  let f = Filter (expr e)
  modify $ \s -> s { pattern = appendPattern f (pattern s) }
  return f

bind :: Expr -> Variable -> Query Pattern
bind e v = do
  let b = Bind (expr e) v
  modify $ \s -> s { pattern = appendPattern b (pattern s) }
  return b

-- Random auxiliary

-- |Form a 'Node', with the 'Prefix' and reference name.
(.:.) :: Prefix -> T.Text -> IRIRef
(.:.) = PrefixedName


-- Duplicate handling

-- |Set duplicate handling to 'Distinct'. By default, there are no reductions.
distinct :: Query Duplicates
distinct = do modify $ \s -> s { duplicates = Distinct }
              gets duplicates

-- |Set duplicate handling to 'Reduced'. By default, there are no reductions.
reduced :: Query Duplicates
reduced = do modify $ \s -> s { duplicates = Reduced }
             gets duplicates

-- |Set limit handling to the given value.  By default, there are no limits.
--  Note: negative numbers cause no query results to be returned.
limit :: Int -> Query Limit
limit n = do modify $ \s -> s { limits = Limit n }
             gets limits

-- Order handling

-- |Alias of 'orderNextAsc'.
orderNext :: (TermLike a) => a -> Query ()
orderNext = orderNextAsc

-- |Order the results, after any previous ordering, based on the term, in
--  ascending order.
orderNextAsc :: (TermLike a) => a -> Query ()
orderNextAsc x  = modify $ \s -> s { ordering = ordering s ++ [Asc  $ expr x] }

-- |Order the results, after any previous ordering, based on the term, in
--  descending order.
orderNextDesc :: (TermLike a) => a -> Query ()
orderNextDesc x = modify $ \s -> s { ordering = ordering s ++ [Desc $ expr x] }

-- |Permit variables and values to seemlessly be put into argument for 'triple'
--  and similar functions
class TermLike a where
  varOrTerm :: a -> VarOrTerm

  expr :: a -> Expr
  expr = VarOrTermExpr . varOrTerm

instance TermLike Variable where
  varOrTerm = Var

instance TermLike IRIRef where
  varOrTerm = Term . IRIRefTerm

instance TermLike BlankNodePattern where
  varOrTerm [] = Term (BNode Nothing)
  varOrTerm xs = BlankNodePattern' xs

  expr [] = error "FIXME: blank node expression"
  expr _ = error "cannot use a blank node pattern as an expression"

instance TermLike Expr where
  varOrTerm = error "cannot use an expression as a term"
  expr = id

instance TermLike Integer where
  varOrTerm = Term . NumericLiteralTerm
  expr = NumericExpr . NumericLiteralExpr

instance TermLike T.Text where
  varOrTerm = Term . RDFLiteralTerm . RDF.plainL

instance TermLike (T.Text, T.Text) where
  varOrTerm (s, lang') = Term . RDFLiteralTerm $ RDF.plainLL s lang'

instance TermLike (T.Text, IRIRef) where
  varOrTerm (s, ref) = Term . RDFLiteralTerm $ RDF.typedL s (getFQN ref)

instance TermLike Bool where
  varOrTerm = Term . BooleanLiteralTerm

instance TermLike RDF.Node where
  varOrTerm n@(RDF.UNode _)  = Term . IRIRefTerm . AbsoluteIRI $ n
  varOrTerm (RDF.LNode lv)   = Term . RDFLiteralTerm $ lv
  varOrTerm (RDF.BNode i)    = Term . BNode . Just $ i
  varOrTerm (RDF.BNodeGen i) = Term . BNode . Just . T.pack . mconcat $ ["genid", show i]

instance TermLike VarOrNode where
  varOrTerm (Var' v) = Var v
  varOrTerm (RDFNode n) = varOrTerm n

-- |Restriction of TermLike to the role of subject.
class (TermLike a) => SubjectTermLike a

instance SubjectTermLike IRIRef
instance SubjectTermLike Variable
instance SubjectTermLike BlankNodePattern

-- |Restriction of TermLike to the role of predicate.
class (TermLike a) => PredicateTermLike a

instance PredicateTermLike IRIRef
instance PredicateTermLike Variable

-- |Restriction of TermLike to the role of object.
class (TermLike a) => ObjectTermLike a

instance ObjectTermLike IRIRef
instance ObjectTermLike Variable
instance ObjectTermLike BlankNodePattern
instance ObjectTermLike Expr
instance ObjectTermLike Integer
instance ObjectTermLike T.Text
instance ObjectTermLike (T.Text, T.Text)
instance ObjectTermLike (T.Text, IRIRef)
instance ObjectTermLike Bool
instance ObjectTermLike VarOrNode
instance ObjectTermLike RDF.Node

-- Operations
operation :: (TermLike a, TermLike b) => Operation -> a -> b -> Expr
operation op x y = NumericExpr $ OperationExpr op (expr x) (expr y)

-- |Add two terms.
(.+.) :: (TermLike a, TermLike b) => a -> b -> Expr
(.+.) = operation Add

-- |Find the difference between two terms.
(.-.) :: (TermLike a, TermLike b) => a -> b -> Expr
(.-.) = operation Subtract

-- |Multiply two terms.
(.*.) :: (TermLike a, TermLike b) => a -> b -> Expr
(.*.) = operation Multiply

-- |Divide two terms.
(./.) :: (TermLike a, TermLike b) => a -> b -> Expr
(./.) = operation Divide

-- | Combine two boolean terms with AND
(.&&.) :: (TermLike a, TermLike b) => a -> b -> Expr
(.&&.) = operation And

-- | Combine two boolean terms with OR
(.||.) :: (TermLike a, TermLike b) => a -> b -> Expr
(.||.) = operation Or

infixr 2 .||.
infixr 3 .&&.
infixl 7 .*.
infixl 7 ./.
infixl 6 .+.
infixl 6 .-.

-- Relations
relation :: (TermLike a, TermLike b) => Relation -> a -> b -> Expr
relation rel x y = RelationalExpr rel (expr x) (expr y)

-- |Create an expression which tests the relationship of the two operands,
--  evaluating their equivalence.
(.==.) :: (TermLike a, TermLike b) => a -> b -> Expr
(.==.) = relation Equal

-- |Create an expression which tests the relationship of the two operands,
--  evaluating their equivalence.
(.!=.) :: (TermLike a, TermLike b) => a -> b -> Expr
(.!=.) = relation NotEqual

-- |Create an expression which tests the relationship of the two operands,
--  evaluating their relative value.
(.<.) :: (TermLike a, TermLike b) => a -> b -> Expr
(.<.) = relation LessThan

-- |Create an expression which tests the relationship of the two operands,
--  evaluating their relative value.
(.>.) :: (TermLike a, TermLike b) => a -> b -> Expr
(.>.) = relation GreaterThan

-- |Create an expression which tests the relationship of the two operands,
--  evaluating their relative value.
(.<=.) :: (TermLike a, TermLike b) => a -> b -> Expr
(.<=.) = relation LessThanOrEqual

-- |Create an expression which tests the relationship of the two operands,
--  evaluating their relative value.
(.>=.) :: (TermLike a, TermLike b) => a -> b -> Expr
(.>=.) = relation GreaterThanOrEqual

infix 4 .==., .!=., .<., .<=., .>., .>=.

-- Negation

-- |Negate any term-like expression, for use, e.g., in filtering.
notExpr :: (TermLike a) => a -> Expr
notExpr = NegatedExpr . expr

-- Builtin Functions
type BuiltinFunc1 = forall a . (TermLike a) => a -> Expr
builtinFunc1 :: Function -> BuiltinFunc1
builtinFunc1 f x = BuiltinCall f [expr x]

type BuiltinFunc2 = forall a b . (TermLike a, TermLike b) => a -> b -> Expr
builtinFunc2 :: Function -> BuiltinFunc2
builtinFunc2 f x y = BuiltinCall f [expr x, expr y]

type BuiltinFunc3 = forall a b c . (TermLike a, TermLike b, TermLike c) => a -> b -> c -> Expr
builtinFunc3 :: Function -> BuiltinFunc3
builtinFunc3 f x y z = BuiltinCall f [expr x, expr y, expr z]

str :: BuiltinFunc1
str = builtinFunc1 StrFunc

lang :: BuiltinFunc1
lang = builtinFunc1 LangFunc

langMatches :: BuiltinFunc2
langMatches = builtinFunc2 LangMatchesFunc

datatype :: BuiltinFunc1
datatype = builtinFunc1 DataTypeFunc

bound :: Variable -> Expr
bound x = BuiltinCall BoundFunc [expr x]

sameTerm :: BuiltinFunc2
sameTerm = builtinFunc2 SameTermFunc

isIRI :: BuiltinFunc1
isIRI = builtinFunc1 IsIRIFunc

isURI :: BuiltinFunc1
isURI = builtinFunc1 IsURIFunc

isBlank :: BuiltinFunc1
isBlank = builtinFunc1 IsBlankFunc

isLiteral :: BuiltinFunc1
isLiteral = builtinFunc1 IsLiteralFunc

regex :: BuiltinFunc2
regex = builtinFunc2 RegexFunc

regexOpts :: BuiltinFunc3
regexOpts = builtinFunc3 RegexFunc

-- Default QueryData
queryData :: QueryData
queryData = QueryData
    { prefixes   = []
    , varsIdx    = 0
    , vars       = []
    , queryType  = TypeNotSet
    , pattern    = GroupGraphPattern []
    , constructTriples = []
    , askTriples = []
    , updateTriples = []
    , describeURI = Nothing
    , duplicates = NoLimits
    , limits     = NoLimit
    , ordering   = []
    }


-- Query representation
class QueryShow a where
  -- |Convert most query-related types to a 'String', most importantly
  --  'QueryData's.
  qshow :: a -> String

data Duplicates = NoLimits | Distinct | Reduced
                deriving (Show)

data Limit = NoLimit | Limit Int
           deriving (Show)

data Prefix = Prefix T.Text RDF.Node
            deriving (Show)

data Variable = Variable Int
              deriving (Show)

data DynamicPredicate = forall a. (PredicateTermLike a, QueryShow a, Show a) => DynamicPredicate a
data DynamicObject = forall a. (ObjectTermLike a, QueryShow a, Show a) => DynamicObject a
type DynamicPredicateObject = (DynamicPredicate, DynamicObject)
type BlankNodePattern = [DynamicPredicateObject]

instance Show DynamicPredicate where
  show (DynamicPredicate a) = show a

instance Show DynamicObject where
  show (DynamicObject a) = show a

-- |support for blank nodes.
--
-- Define a convenient alias for `mkPredicateObject`. Note: a
-- pointfree definition leads to the monomorphism restriction @(&) =
-- mkPredicateObject@. An example of its use:
--
-- > p & o = mkPredicateObject p o
--
-- for example
--
-- > q = do
-- >   p <- prefix "" (iriRef "http://example.com/")
-- >   s <- var
-- >   o1 <- var
-- >   o2 <- var
-- >   _ <- triple s (p .:. "p1") [(p .:. "p2") & [(p .:. "p3") & o1], (p .:. "p4") & o2]
-- >   return SelectQuery { queryVars = [s, o1] }
--
-- >>> createSelectQuery q
-- "PREFIX : <http://example.com/> SELECT  ?x0 ?x1 WHERE {?x0 :p1 [:p2 [:p3 ?x1]], [:p4 ?x2] .} "
mkPredicateObject :: (PredicateTermLike a, ObjectTermLike b, QueryShow a, QueryShow b, Show a, Show b) => a -> b -> DynamicPredicateObject
mkPredicateObject p o = (DynamicPredicate p, DynamicObject o)

data IRIRef = AbsoluteIRI RDF.Node
            | PrefixedName Prefix T.Text
            deriving (Show)

iriRef :: T.Text -> IRIRef
iriRef uri = AbsoluteIRI $ RDF.unode uri

getFQN :: IRIRef -> T.Text
getFQN (AbsoluteIRI (RDF.UNode n)) = n
getFQN (PrefixedName (Prefix _ (RDF.UNode n)) s) = T.append n s
-- FIXME
getFQN _ = error "getFQN: input not supported"

-- FIXME: Should support numeric literals, too
data GraphTerm = IRIRefTerm IRIRef
               | RDFLiteralTerm RDF.LValue
               | NumericLiteralTerm Integer
               | BooleanLiteralTerm Bool
               | BNode (Maybe T.Text)
               deriving (Show)

data VarOrTerm = Var Variable
               | Term GraphTerm
               | BlankNodePattern' BlankNodePattern
               deriving (Show)

-- |Enables programmatic construction of triples where it is not known in
-- advance which parts of the triple will be variables and which will be
-- 'Node's.
data VarOrNode = Var' Variable
               | RDFNode RDF.Node
               deriving (Show)

data Operation = Add | Subtract | Multiply | Divide | And | Or
               deriving (Show)

data NumericExpr = NumericLiteralExpr Integer
                 | OperationExpr Operation Expr Expr
                 deriving (Show)

data Relation = Equal | NotEqual | LessThan | GreaterThan | LessThanOrEqual | GreaterThanOrEqual
              deriving (Show)

data Function = StrFunc | LangFunc | LangMatchesFunc | DataTypeFunc | BoundFunc
              | SameTermFunc | IsIRIFunc | IsURIFunc | IsBlankFunc
              | IsLiteralFunc | RegexFunc
              deriving (Show)

data Expr = OrExpr [Expr]
          | AndExpr [Expr]
          | NegatedExpr Expr
          | RelationalExpr Relation Expr Expr
          | NumericExpr NumericExpr
          | BuiltinCall Function [Expr]
          | VarOrTermExpr VarOrTerm
          deriving (Show)

data Pattern = QTriple VarOrTerm VarOrTerm VarOrTerm
             | Filter Expr
             | Bind Expr Variable
             | OptionalGraphPattern GroupGraphPattern
             | UnionGraphPattern GroupGraphPattern GroupGraphPattern

data GroupGraphPattern = GroupGraphPattern [Pattern]

data OrderBy = Asc Expr
             | Desc Expr

-- Auxiliary, but fairly useful
-- TODO don't add to end
appendPattern :: Pattern -> GroupGraphPattern -> GroupGraphPattern
appendPattern p (GroupGraphPattern ps) = GroupGraphPattern (ps ++ [p])

appendTriple :: a -> [a] -> [a]
appendTriple t ts = t : ts

data QueryData = QueryData
    { prefixes   :: [Prefix]
    , varsIdx    :: Int
    , vars       :: [Variable]
    , queryType  :: QueryType
    , pattern    :: GroupGraphPattern
    , constructTriples :: [Pattern] -- QTriple
    , askTriples :: [Pattern]
    , updateTriples :: [Pattern]
    , describeURI :: Maybe IRIRef
    , duplicates :: Duplicates
    , limits     :: Limit
    , ordering   :: [OrderBy]
    }


data QueryType = SelectType | ConstructType | AskType | UpdateType | DescribeType | TypeNotSet

data QueryForm = SelectForm QueryData | ConstructForm QueryData | AskForm QueryData | UpdateForm QueryData | DescribeForm QueryData

data ConstructQuery = ConstructQuery
    { queryConstructs :: [Pattern] }

data AskQuery = AskQuery
    { queryAsk :: [Pattern] }

data UpdateQuery = UpdateQuery
    { queryUpdate :: [Pattern] }

data SelectQuery = SelectQuery
    { queryVars :: [Variable] }

data DescribeQuery = DescribeQuery
    { queryDescribe :: IRIRef }


-- QueryShow instances
instance QueryShow BlankNodePattern where
  qshow [] = "[]"
  qshow xs = intercalate ", " $ fmap qshow xs

instance QueryShow DynamicPredicateObject where
  qshow (DynamicPredicate p, DynamicObject o) = mconcat ["[", qshow p, " ", qshow o, "]"]

instance QueryShow Duplicates where
  qshow NoLimits = ""
  qshow Distinct = "DISTINCT"
  qshow Reduced  = "REDUCED"

instance QueryShow Limit where
  qshow NoLimit   = ""
  qshow (Limit n) = "Limit " ++ show n

instance QueryShow RDF.Node where
  qshow (RDF.UNode n) = "<" ++ T.unpack n ++ ">"
  qshow (RDF.BNode n) = "_:" ++ T.unpack n
  qshow (RDF.BNodeGen i) = "_:genid" ++ show i
  qshow (RDF.LNode n) = qshow n

instance QueryShow RDF.LValue where
  qshow (RDF.PlainL lit)       = T.unpack . T.concat $ ["\"", escapeSpecialChar lit, "\""]
  qshow (RDF.PlainLL lit lang_) = T.unpack . T.concat $ ["\"", escapeSpecialChar lit, "\"@", lang_]
  qshow (RDF.TypedL lit dtype) = T.unpack . T.concat $ ["\"", escapeSpecialChar lit, "\"^^<", dtype, ">"]

instance QueryShow Prefix where
  qshow (Prefix pre ref) = "PREFIX " ++ (T.unpack pre) ++ ": " ++ qshow ref

instance QueryShow [Prefix] where
  qshow = unwords . fmap qshow

instance QueryShow Variable where
  qshow (Variable v) = "?x" ++ show v

instance QueryShow [Variable] where
  qshow = unwords . fmap qshow

instance QueryShow IRIRef where
  qshow (AbsoluteIRI n) = qshow n
  qshow (PrefixedName (Prefix pre _) s) = (T.unpack pre) ++ ":" ++ (T.unpack s)

instance QueryShow (Maybe IRIRef) where
  qshow (Just r) = qshow r
  qshow Nothing = ""

instance QueryShow GraphTerm where
  qshow (IRIRefTerm ref)           = qshow ref
  qshow (RDFLiteralTerm s)         = qshow s
  qshow (BooleanLiteralTerm True)  = show ("true" :: String)
  qshow (BooleanLiteralTerm False) = show ("false" :: String)
  qshow (NumericLiteralTerm i)     = show i
  qshow (BNode Nothing)            = "[]"
  qshow (BNode (Just i))           = "_:" ++ T.unpack i

instance QueryShow VarOrTerm where
  qshow (Var  v) = qshow v
  qshow (Term t) = qshow t
  qshow (BlankNodePattern' bn) = qshow bn

instance QueryShow [VarOrTerm] where
  qshow = unwords . fmap qshow

instance QueryShow Operation where
  qshow Add      = "+"
  qshow Subtract = "-"
  qshow Multiply = "*"
  qshow Divide   = "/"
  qshow And      = "&&"
  qshow Or       = "||"

instance QueryShow NumericExpr where
  qshow (NumericLiteralExpr n) = show n
  qshow (OperationExpr op x y) = qshow x ++ qshow op ++ qshow y

instance QueryShow Relation where
  qshow Equal              = "="
  qshow NotEqual           = "!="
  qshow LessThan           = "<"
  qshow GreaterThan        = ">"
  qshow LessThanOrEqual    = "<="
  qshow GreaterThanOrEqual = ">="

instance QueryShow Function where
  qshow StrFunc         = "STR"
  qshow LangFunc        = "LANG"
  qshow LangMatchesFunc = "LANGMATCHES"
  qshow DataTypeFunc    = "DATATYPE"
  qshow BoundFunc       = "BOUND"
  qshow SameTermFunc    = "sameTerm"
  qshow IsIRIFunc       = "isIRI"
  qshow IsURIFunc       = "isURI"
  qshow IsBlankFunc     = "isBlank"
  qshow IsLiteralFunc   = "isLiteral"
  qshow RegexFunc       = "REGEX"

instance QueryShow Expr where
  qshow = qshow'
    where qshow' (VarOrTermExpr vt) = qshow vt
          qshow' (OrExpr es)        = wrap $ intercalate " || " $ map qshow es
          qshow' (AndExpr es)       = wrap $ intercalate " && " $ map qshow es
          qshow' (NegatedExpr e')   = wrap $ '!' : qshow e'
          qshow' (RelationalExpr rel e1 e2) = wrap $ qshow e1 ++ qshow rel ++ qshow e2
          qshow' (NumericExpr e')   = wrap $ qshow e'
          qshow' (BuiltinCall f es) = wrap $ qshow f ++ "(" ++ intercalate ", " (map qshow es) ++ ")"
          wrap e = "(" ++ e ++ ")"

instance QueryShow Pattern where
  qshow (QTriple a b c) = intercalate " " [qshow a, qshow b, qshow c, "."]
  qshow (Filter e)      = "FILTER " ++ qshow e ++ " ."
  qshow (Bind e v)      = "BIND(" ++ qshow e ++ " AS " ++ qshow v ++ ")"
  qshow (OptionalGraphPattern p)  = "OPTIONAL " ++ qshow p
  qshow (UnionGraphPattern p1 p2) = qshow p1 ++ " UNION " ++ qshow p2

instance QueryShow [Pattern] where
  qshow = unwords . fmap qshow

instance QueryShow GroupGraphPattern where
  qshow (GroupGraphPattern ps) = "{" ++ qshow ps ++ "}"

instance QueryShow OrderBy where
  qshow (Asc e)  = "ASC(" ++ qshow e ++ ")"
  qshow (Desc e) = "DESC(" ++ qshow e ++ ")"

instance QueryShow QueryForm where
  qshow (SelectForm qd) =  unwords
                        [ "SELECT"
                        , qshow (duplicates qd)
                        , qshow (vars qd)
                        ]
  qshow (ConstructForm qd) = "CONSTRUCT { " ++ qshow (constructTriples qd) ++ " }"
  qshow (AskForm qd) = "ASK { " ++ qshow (askTriples qd) ++ " }"
  qshow (UpdateForm qd) = "INSERT DATA { " ++ qshow (updateTriples qd) ++ " }"
  qshow (DescribeForm qd) = "DESCRIBE " ++ qshow (describeURI qd)

instance QueryShow QueryData where
  qshow qd = let whereStmt = unwords $
                              ["WHERE"
                              , qshow (pattern qd)
                              ] ++ case ordering qd of
                                      [] -> []
                                      os -> "ORDER BY" : map qshow os

                 query = case queryType qd of
                  SelectType ->
                   unwords [ qshow (prefixes qd)
                           , qshow (SelectForm qd)
                           , whereStmt
                           , qshow (limits qd)
                           ]
                  ConstructType ->
                   unwords [ qshow (prefixes qd)
                           , qshow (ConstructForm qd)
                           , whereStmt
                           ]
                  DescribeType ->
                   unwords [ qshow (prefixes qd)
                           , qshow (DescribeForm qd)
                           , whereStmt
                           ]
                  AskType ->
                   unwords [ qshow (prefixes qd)
                           , qshow (AskForm qd)
                           ]
                  UpdateType ->
                   unwords [ qshow (prefixes qd)
                           , qshow (UpdateForm qd)
                           ]
                  -- FIXME
                  TypeNotSet ->
                    error "instance QueryShow QueryData: TypeNotSet not supported."
             in query


-- Internal utilities
escapeSpecialChar :: T.Text -> T.Text
escapeSpecialChar = T.concatMap handleChar
  -- FIXME: probably more cases to handle
  where handleChar '\n' = "\n"
        handleChar '\t' = "\t"
        handleChar '\r' = "\r"
        handleChar '"'  = "\\\""
        handleChar '\\' = "\\\\"
        handleChar c    = T.singleton c
