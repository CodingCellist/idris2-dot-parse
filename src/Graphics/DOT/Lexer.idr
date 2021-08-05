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

  ||| Any amount of whitespace
  Whitespace : (ws : String) -> DOTToken

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
  show (Whitespace ws) = "WS"
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
nodeKW = approx "node"

edgeKW : Lexer
edgeKW = approx "edge"

graphKW : Lexer
graphKW = approx "graph"

digraphKW : Lexer
digraphKW = approx "digraph"

subgraphKW : Lexer
subgraphKW = approx "subgraph"

strictKW : Lexer
strictKW = approx "strict"

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
nameID = nameIDHead <+> many nameIDRest

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
numeralID = opt minus <+> numeral_helper

||| Any double-quoted string possibly containing escaped quotes.
stringID : Lexer
stringID = stringLit

--  v  this is probably a library on its own...
||| An HTML string
htmlID : Lexer
htmlID = ?todo_htmlIDs_are_not_implemented_atm


-- Edge operations --

digraphEdgeOp : Lexer
digraphEdgeOp = exact "->"

graphEdgeOp : Lexer
graphEdgeOp = exact "--"


-- Compass Points --

northCPt : Lexer
northCPt =        is 'n'

northEastCPt : Lexer
northEastCPt = exact "ne"

eastCPt : Lexer
eastCPt =         is 'e'

southEastCPt : Lexer
southEastCPt = exact "se"

southCPt : Lexer
southCPt =        is 'e'

southWestCPt : Lexer
southWestCPt = exact "sw"

westCPt : Lexer
westCPt =         is 'w'

northWestCPt : Lexer
northWestCPt = exact "nw"

centerCPt : Lexer
centerCPt =       is 'c'

-- The compass point "_" specifies that an appropriate side of the port adjacent
-- to the exterioior of the node should be used, if such exists. Otherwise, the
-- center is used. If no compass point is used with a portname, the default
-- value is "_".
underCPt : Lexer
underCPt =        is '_'

-- (n | ne | e | se | s | sw | w | nw | c | _)
compassPt : Lexer
compassPt = northCPt
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
cPreProcessorOutput = lineComment (is '#')

cppLineComment : Lexer
cppLineComment = lineComment (exact "//")

cppBlockComment : Lexer
cppBlockComment = blockComment (exact "/*") (exact "*/")


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
multilineBackslash = (is '\\') <+> newline

-- double-quoted strings can be concatenated using a '+' operator
strConcat : Lexer
strConcat = is '+'

whitespace : Lexer
whitespace = spaces

lBracket : Lexer
lBracket = is '['

rBracket : Lexer
rBracket = is ']'

lBrace : Lexer
lBrace = is '{'

rBrace : Lexer
rBrace = is '}'

comma : Lexer
comma = is ','

semicolon : Lexer
semicolon = is ';'

colon : Lexer
colon = is ':'

equal : Lexer
equal = is '='


---------------
-- TOKEN MAP --
---------------

||| A mapping from the @Lexer@s to a function of type String -> @DOTToken@
dotTokenMap : TokenMap DOTToken
dotTokenMap = [ (keyword,             \str => Keyword (toLower str))
              , (nameID,              \name => NameID name)
              , (numeralID,           \numeral => NumeralID numeral)
              , (stringID,            \str => StringID str)
              , (htmlID,              \html => HTML_ID html)
              , (digraphEdgeOp,       const DiGrEdgeOp)
              , (graphEdgeOp,         const GrEdgeOp)
              , (compassPt,           \pt  => CompassPt pt)
              , (comment,             \str => Comment str)
              , (multilineBackslash,  const MultilineBackslash)
              , (strConcat,           const StrConcat)
              , (whitespace,          \str => Whitespace str)
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

