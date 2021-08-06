module Graphics.DOT.Lexer

import Text.Lexer
import Data.String

%default total

public export
data DOTToken : Type where
  ||| A keyword
  Keyword : (kw : String) -> DOTToken

  ||| An identifier as a name
  NameID : (name : String) -> DOTToken
  ||| An identifier as a numeral
  NumeralID : (numeral : String) -> DOTToken
  ||| An identifier as a double-quoted string
  StringID : (str : String) -> DOTToken
  ||| An identifier as a HTML string
  HTML_ID : (html : String) -> DOTToken

  ||| An edge operation in a directed graph
  DiGrEdgeOp : DOTToken
  ||| An edge operation in a graph
  GrEdgeOp : DOTToken
  ||| A compass point (used in combination with ports)
  CompassPt : (pt : String) -> DOTToken
  ||| A comment
  Comment : (contents : String) -> DOTToken
  ||| A multiline indicator
  MultilineBackslash : DOTToken
  ||| A double-quoted-string concatenation operator ('+')
  StrConcat : DOTToken

  ||| [
  LBracket : DOTToken
  ||| ]
  RBracket : DOTToken
  ||| {
  LBrace : DOTToken
  ||| }
  RBrace : DOTToken
  ||| ,
  Comma : DOTToken
  ||| ;
  Semicolon : DOTToken
  ||| :
  Colon : DOTToken
  ||| =
  Equal : DOTToken

export
Show DOTToken where
  show (Keyword kw) = "(KW " ++ kw ++ ")"
  show (NameID name) = "(NameID " ++ name ++ ")"
  show (NumeralID numeral) = "(NumeralID " ++ numeral ++ ")"
  show (StringID str) = "(StringID " ++ str ++ ")"
  show (HTML_ID html) = "(HTML_ID " ++ html ++ ")"
  show DiGrEdgeOp = "DiEO"
  show GrEdgeOp = "GrEO"
  show (CompassPt pt) = "(CPt " ++ pt ++ ")"
  show (Comment contents) = "(COM " ++ contents ++ ")"
  show MultilineBackslash = "MLB"
  show StrConcat = "STR++"
  show LBracket = "LBracket"
  show RBracket = "RBracket"
  show LBrace = "LBrace"
  show RBrace = "RBrace"
  show Comma = "Comma"
  show Semicolon = "Semicolon"
  show Colon = "Colon"
  show Equal = "EQ"

-- Keywords --

-- DOT keywords are case-insensitive

nodeKW : Lexer
nodeKW = manyThen (approx "node") space

edgeKW : Lexer
edgeKW = manyThen (approx "edge") space

graphKW : Lexer
graphKW = manyThen (approx "graph") space

digraphKW : Lexer
digraphKW = manyThen (approx "digraph") space

subgraphKW : Lexer
subgraphKW = manyThen (approx "subgraph") space

strictKW : Lexer
strictKW = manyThen (approx "strict") space

||| Keywords are "node", "edge", "graph", "digraph", "subgraph", and "strict"
||| (without the quotes)
keyword : Lexer
keyword =  nodeKW
       <|> edgeKW
       <|> graphKW
       <|> digraphKW
       <|> subgraphKW
       <|> strictKW


-- Identifiers --

underscore : Lexer
underscore = is '_'

nameIDHead : Lexer
nameIDHead =  alpha
          <|> underscore

nameIDRest : Lexer
nameIDRest =  alphaNum
          <|> underscore

||| Any string of alphabetic characters, underscores, or digits, not beginning
||| with a digit.
nameID : Lexer
nameID = manyThen (nameIDHead <+> many nameIDRest) space

minus : Lexer
minus = is '-'

fullStop : Lexer
fullStop = is '.'

-- \.[0-9]+
dotNum : Lexer
dotNum = fullStop <+> digits

-- \.[0-9]*
decimalNumRest : Lexer
decimalNumRest = fullStop <+> many digit

-- [0-9]+(\.[0-9]*)?
decimalNum : Lexer
decimalNum = digits <+> opt decimalNumRest

-- \.[0-9]+|[0-9]+(\.[0-9]*)?
numeral_helper : Lexer
numeral_helper =  dotNum
              <|> decimalNum

||| A numeral:
||| [-]?(\.[0-9]+|[0-9]+(\.[0-9]*)?)
numeralID : Lexer
numeralID = manyThen (opt minus <+> numeral_helper) space

||| Any double-quoted string possibly containing escaped quotes.
stringID : Lexer
stringID = manyThen (stringLit) space

--  v  this is probably a library on its own...
||| An HTML string
htmlID : Lexer
htmlID = manyThen (?todo_htmlIDs_are_not_implemented_atm) space


-- Edge operations --

digraphEdgeOp : Lexer
digraphEdgeOp = manyThen (exact "->") space

graphEdgeOp : Lexer
graphEdgeOp = manyThen (exact "--") space


-- Compass Points --

northCPt : Lexer
northCPt =
  manyThen (is 'n') space

northEastCPt : Lexer
northEastCPt =
  manyThen (exact "ne") space

eastCPt : Lexer
eastCPt =
  manyThen (is 'e') space

southEastCPt : Lexer
southEastCPt =
  manyThen (exact "se") space

southCPt : Lexer
southCPt =
  manyThen (is 'e') space

southWestCPt : Lexer
southWestCPt =
  manyThen (exact "sw") space

westCPt : Lexer
westCPt =
  manyThen (is 'w') space

northWestCPt : Lexer
northWestCPt =
  manyThen (exact "nw") space

centerCPt : Lexer
centerCPt =
  manyThen (is 'c') space

-- The compass point "_" specifies that an appropriate side of the port adjacent
-- to the exterioior of the node should be used, if such exists. Otherwise, the
-- center is used. If no compass point is used with a portname, the default
-- value is "_".
underCPt : Lexer
underCPt =
  manyThen (is '_') space

-- (n | ne | e | se | s | sw | w | nw | c | _)
compassPt : Lexer
compassPt =  northCPt
         <|> northEastCPt
         <|> eastCPt
         <|> southEastCPt
         <|> southCPt
         <|> southWestCPt
         <|> westCPt
         <|> northWestCPt
         <|> centerCPt
         <|> underCPt


-- Comments --

-- "a line beginning with a '#' character is considered a line output from a C
-- preprocessor and discarded"
cPreProcessorOutput : Lexer
cPreProcessorOutput = manyThen (lineComment (is '#')) space

cppLineComment : Lexer
cppLineComment = manyThen (lineComment (exact "//")) space

cppBlockComment : Lexer
cppBlockComment = manyThen (blockComment (exact "/*") (exact "*/")) space


||| The language supports C++-style comments `/* */` and `//`. Line starting
||| with a '#' are considered pre-processer output and ignored.
comment : Lexer
comment =  cppLineComment
       <|> cppBlockComment
       <|> cPreProcessorOutput


-- Misc --

-- DOT allows double-quoted strings to span multiple physical lines using the
-- standard C convention of a backslash immediately preceding a newline
-- character
multilineBackslash : Lexer
multilineBackslash = manyThen ((is '\\') <+> newline) space

-- double-quoted strings can be concatenated using a '+' operator
strConcat : Lexer
strConcat = manyThen (is '+') space

lBracket : Lexer
lBracket = manyThen (is '[') space

rBracket : Lexer
rBracket = manyThen (is ']') space

lBrace : Lexer
lBrace = manyThen (is '{') space

rBrace : Lexer
rBrace = manyThen (is '}') space

comma : Lexer
comma = manyThen (is ',') space

semicolon : Lexer
semicolon = manyThen (is ';') space

colon : Lexer
colon = manyThen (is ':') space

equal : Lexer
equal = manyThen (is '=') space


---------------
-- TOKEN MAP --
---------------

||| A mapping from the @Lexer@s to a function of type String -> @DOTToken@
dotTokenMap : TokenMap DOTToken
dotTokenMap = [ (keyword,             \str => Keyword (toLower str))
              , (nameID,              \name => NameID name)
              , (numeralID,           \numeral => NumeralID numeral)
              , (stringID,            \str => StringID str)
--              , (htmlID,              \html => HTML_ID html)
              , (digraphEdgeOp,       const DiGrEdgeOp)
              , (graphEdgeOp,         const GrEdgeOp)
              , (compassPt,           \pt  => CompassPt pt)
              , (comment,             \str => Comment str)
              , (multilineBackslash,  const MultilineBackslash)
              , (strConcat,           const StrConcat)
              , (lBracket,            const LBracket)
              , (rBracket,            const RBracket)
              , (lBrace,              const LBrace)
              , (rBrace,              const RBrace)
              , (comma,               const Comma)
              , (semicolon,           const Semicolon)
              , (colon,               const Colon)
              , (equal,               const Equal)
              ]

||| Given a source file, return the token stream
export
lex : String -> (List (TokenData DOTToken), (Int, (Int, String)))
lex fStr = lex dotTokenMap fStr

