module Graphics.DOT.Parser

import Text.Parser
import Data.String
import Data.Vect

import Graphics.DOT.Lexer
import Graphics.DOT.AST

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

nodeKW : Grammar DOTToken True DOT
nodeKW = terminal "Expected 'node' keyword"
          (\case Keyword "node" => Just Node
                 _ => Nothing)

edgeKW : Grammar DOTToken True DOT
edgeKW = terminal "Expecetd 'edge' keyword"
          (\case Keyword "edge" => Just Edge
                 _ => Nothing)

graphKW : Grammar DOTToken True DOT
graphKW = terminal "Expected 'graph' keyword"
           (\case Keyword "graph" => Just Graph
                  _ => Nothing)

digraphKW : Grammar DOTToken True DOT
digraphKW = terminal "Expected 'digraph' keyword"
              (\case Keyword "digraph" => Just Graph
                     _ => Nothing)

subgraphKW : Grammar DOTToken True DOT
subgraphKW = terminal "Expected 'subgraph' keyword"
              (\case Keyword "subgraph" => Just SubGraph
                     _ => Nothing)

strictKW : Grammar DOTToken True DOT
strictKW = terminal "Expected 'strict' keyword"
            (\case Keyword "strict" => Just Strict
                   _ => Nothing)

||| Keywords ('node', 'edge', 'graph', 'digraph', 'subgraph', 'strict').
keyword : Grammar DOTToken True DOT
keyword =  nodeKW
       <|> edgeKW
       <|> graphKW
       <|> digraphKW
       <|> subgraphKW
       <|> strictKW

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

a_list' : Grammar DOTToken True (List DOT)
a_list' =  (do head <- assign_
               sepChoice
               rest <- a_list'
               pure (head :: rest))
       <|> (do head <- assign_
               sepChoice
               pure (head :: []))

||| An 'a_list' is an assignment, optionally followed by a separator, optionally
||| followed by more of an 'a_list' (see helper `a_list'`).
a_list : Grammar DOTToken True DOT
a_list = do l <- a_list'
            pure (AList l)

-- OLD IMPLEMENTATION:
--a_list = (do head <- assign_
--             sepChoice
--             pure (AList [head]))
--      <|> (do head <- assign_
--              sepChoice
--              rest <- a_list
--              pure (AList (head :: [rest])))

attr_list' : Grammar DOTToken True (List DOT)
attr_list' =  (do lBracket
                  Just aList <- optional a_list
                    | Nothing => do rBracket
                                    rest <- attr_list'
                                    pure rest
                  rBracket
                  rest <- attr_list'
                  pure (aList :: rest))
          <|> (do lBracket
                  Just aList <- optional a_list
                    | Nothing => do rBracket
                                    pure []
                  rBracket
                  pure [aList])

||| An 'attr_list' is a '[', optionally followed by an 'a_list', followed by a
||| ']', optionally followed by another 'attr_list' (see helper `attr_list'`).
attr_list : Grammar DOTToken True DOT
attr_list = do l <- attr_list'
               pure (AttrList l)

-- OLD IMPLEMENTATION:
--attr_list =  (do lBracket
--                 mAlist <- optional a_list
--                 rBracket
--                 pure (AttrList (maybeToList mAlist)))
--         <|> (do lBracket
--                 mAlist <- optional a_list
--                 rBracket
--                 rest <- attr_list
--                 pure (AttrList ((maybeToList mAlist) ++ [rest])))
--          where
--            maybeToList : Maybe DOT -> List DOT
--            maybeToList Nothing    = []
--            maybeToList (Just dot) = [dot]

||| An attr_stmt is one of the keywords 'graph', 'node', or 'edge', followed by
||| an attr_list.
attr_stmt : Grammar DOTToken True DOT
attr_stmt = do kw <- gne
               attrList <- attr_list
               pure (AttrStmt kw attrList)
  where
    gne : Grammar DOTToken True DOT
    gne =  graphKW
       <|> nodeKW
       <|> edgeKW

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

