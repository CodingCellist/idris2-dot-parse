module Graphics.DOT.Parser

import Text.Parser
import Data.String
import Data.Vect

import Graphics.DOT.Lexer
import Graphics.DOT.Representation

%default total

-- Terminals --

lBrace : Grammar DOTToken True ()
lBrace = terminal "Expected '{'"
            (\case LBrace => Just ()
                   _ => Nothing)

rBrace : Grammar DOTToken True ()
rBrace = terminal "Expected '}' (might not be properly closed?)"
            (\case RBrace => Just ()
                   _ => Nothing)

lBracket : Grammar DOTToken True ()
lBracket = terminal "Expected '['"
              (\case LBracket => Just ()
                     _ => Nothing)

rBracket : Grammar DOTToken True ()
rBracket = terminal "Expected ']' (might not be properly closed?)"
              (\case RBracket => Just ()
                     _ => Nothing)

colon : Grammar DOTToken True ()
colon = terminal "Expected ':'"
          (\case Colon => Just ()
                 _ => Nothing)

semicolon : Grammar DOTToken True ()
semicolon = terminal "Expected ';' (shouldn't get this message)"
              (\case Semicolon => Just ()
                     _ => Nothing)

comma : Grammar DOTToken True ()
comma = terminal "Expected ','"
          (\case Comma => Just ()
                 _ => Nothing)

equals : Grammar DOTToken True ()
equals = terminal "Expected '='"
          (\case Equal => Just ()
                 _ => Nothing)

nameID : Grammar DOTToken True DOT
nameID = terminal "Not a name"
          (\case (NameID str) => Just (NameID str)
                 _ => Nothing)

numeralID : Grammar DOTToken True DOT
numeralID = terminal "Not a numeral"
              (\case (NumeralID num) => Just (NumeralID num)
                     _ => Nothing)

stringID : Grammar DOTToken True DOT
stringID = terminal "Not a string"
            (\case (StringID str) => Just (StringID str)
                   _ => Nothing)

htmlID : Grammar DOTToken True DOT
htmlID = terminal "Not an HTML string"
          (\case (HTML_ID html) => Just (HTML_ID html)
                 _ => Nothing)

||| Keywords ('node', 'edge', 'graph', 'digraph', 'subgraph', 'strict').
keyword : Grammar DOTToken True DOT
keyword = terminal "Unknown keyword"
            (\case Keyword kw =>
                      case toLower kw of
                           "node" => Just Node
                           "edge" => Just Edge
                           "graph" => Just Graph
                           "digraph" => Just DiGraph
                           "subgraph" => Just SubGraph
                           "strict" => Just Strict
                           _ => Nothing
                   _ => Nothing)

||| Compass points (n, ne, e, se, s, sw, w, nw, c, _).
compassPt : Grammar DOTToken True DOT
compassPt = terminal "Unknown compass-point"
              (\case CompassPt pt =>
                        case pt of
                             "n"  => Just North
                             "ne" => Just NorthEast
                             "e"  => Just East
                             "se" => Just SouthEast
                             "s"  => Just South
                             "sw" => Just SouthWest
                             "w"  => Just West
                             "nw" => Just NorthWest
                             "c"  => Just CenterCPt
                             "_"  => Just UnderCPt
                             _    => Nothing
                     _ => Nothing)

-- Non-terminals --

||| An identifier is either:
||| - a name
||| - a numeral
||| - a quoted string
||| - an HTML string
identifier : Grammar DOTToken True DOT
identifier =  nameID
          <|> numeralID
          <|> stringID
          <|> htmlID

||| Assignment, i.e.
||| ID '=' ID
assign_ : Grammar DOTToken True DOT
assign_ = do idLHS <- identifier
             equals
             idRHS <- identifier
             pure (Assign [idLHS, idRHS])    -- returns assign node in AST

||| Separators are semicolons and commas, but they are purely aesthetic.
sepChoice : Grammar DOTToken False ()
sepChoice = ignore $ optional (choose semicolon comma)

||| An 'a_list' is an assignment, optionally followed by a separator, optionally
||| followed by more of an 'a_list'.
a_list : Grammar DOTToken True DOT
a_list = (do head <- assign_
             sepChoice
             pure (AList [head]))
      <|> (do head <- assign_
              sepChoice
              rest <- a_list
              pure (AList (head :: [rest])))

||| An attr_list is a '[', optionally followed by an a_list, followed by a ']',
||| optionally followed by another attr_list.
attr_list : Grammar DOTToken True DOT
attr_list =  (do lBracket
                 mAlist <- optional a_list
                 rBracket
                 pure (AttrList (maybeToList mAlist)))
         <|> (do lBracket
                 mAlist <- optional a_list
                 rBracket
                 rest <- attr_list
                 pure (AttrList ((maybeToList mAlist) ++ [rest])))
          where
            maybeToList : Maybe DOT -> List DOT
            maybeToList Nothing    = []
            maybeToList (Just dot) = [dot]

||| A colon followed by an ID, optionally followed by more colon+compass_pt
||| pairs.
idPort : Grammar DOTToken True DOT
idPort = do colon
            id_ <- identifier
            maybeCPT <- optional compassPt
            pure (IDPort id_ maybeCPT)

||| A colon followed by a compass_pt.
cptPort : Grammar DOTToken True DOT
cptPort = do colon
             cpt <- compassPt
             pure (CPTPort cpt)

||| A port is either:
||| - A colon followed by an ID, optionally followed by more colon+compass_pt
|||   pairs.
||| - A colon followed by a compass_pt.
port : Grammar DOTToken True DOT
port =  idPort
    <|> cptPort

