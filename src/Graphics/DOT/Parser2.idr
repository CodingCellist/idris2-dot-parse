module Graphics.DOT.Parser2

import Text.Parser

import Data.Vect
import Data.List1
import Data.String

import Graphics.DOT.Lexer
import Graphics.DOT.SemIR

%default total

-- Terminals --

lBrace : Grammar _ DOTToken True ()
lBrace = terminal "Expected '{'"
            (\case LBrace => Just ()
                   _ => Nothing)

rBrace : Grammar _ DOTToken True ()
rBrace = terminal "Expected '}' (might not be properly closed?)"
            (\case RBrace => Just ()
                   _ => Nothing)

lBracket : Grammar _ DOTToken True ()
lBracket = terminal "Expected '['"
              (\case LBracket => Just ()
                     _ => Nothing)

rBracket : Grammar _ DOTToken True ()
rBracket = terminal "Expected ']' (might not be properly closed?)"
              (\case RBracket => Just ()
                     _ => Nothing)

colon : Grammar _ DOTToken True ()
colon = terminal "Expected ':'"
          (\case Colon => Just ()
                 _ => Nothing)

semicolon : Grammar _ DOTToken True ()
semicolon = terminal "Expected ';' (shouldn't get this message)"
              (\case Semicolon => Just ()
                     _ => Nothing)

comma : Grammar _ DOTToken True ()
comma = terminal "Expected ','"
          (\case Comma => Just ()
                 _ => Nothing)

equals : Grammar _ DOTToken True ()
equals = terminal "Expected '='"
          (\case Equal => Just ()
                 _ => Nothing)

nameID : Grammar _ DOTToken True DOTID
nameID = terminal "Not a name"
          (\case (NameID name) => Just (NameID name)
                 _ => Nothing)

numeralID : Grammar _ DOTToken True DOTID
numeralID = terminal "Not a numeral"
              (\case (NumeralID num) => Just (Numeral num)
                     _ => Nothing)

stringID : Grammar _ DOTToken True DOTID
stringID = terminal "Not a string"
            (\case (StringID str) => Just (StringID str)
                   _ => Nothing)

htmlID : Grammar _ DOTToken True DOTID
htmlID = terminal "Not an HTML string"
          (\case (HTML_ID html) => Just (HTML html)
                 _ => Nothing)

nodeKW : Grammar _ DOTToken True Keyword
nodeKW = terminal "Expected 'node' keyword"
          (\case Keyword "node" => Just NodeKW
                 _ => Nothing)

edgeKW : Grammar _ DOTToken True Keyword
edgeKW = terminal "Expecetd 'edge' keyword"
          (\case Keyword "edge" => Just EdgeKW
                 _ => Nothing)

graphKW : Grammar _ DOTToken True Keyword
graphKW = terminal "Expected 'graph' keyword"
           (\case Keyword "graph" => Just GraphKW
                  _ => Nothing)

digraphKW : Grammar _ DOTToken True Keyword
digraphKW = terminal "Expected 'digraph' keyword"
              (\case Keyword "digraph" => Just DigraphKW
                     _ => Nothing)

subgraphKW : Grammar _ DOTToken True Keyword
subgraphKW = terminal "Expected 'subgraph' keyword"
              (\case Keyword "subgraph" => Just SubgraphKW
                     _ => Nothing)

strictKW : Grammar _ DOTToken True Keyword
strictKW = terminal "Expected 'strict' keyword"
            (\case Keyword "strict" => Just StrictKW
                   _ => Nothing)

||| Compass points (n, ne, e, se, s, sw, w, nw, c, _).
compassPt : Grammar _ DOTToken True CompassPoint
compassPt = terminal "Unknown compass-point"
              (\case CompassPt pt =>
                        case pt of
                             "n"  => Just N
                             "ne" => Just NE
                             "e"  => Just E
                             "se" => Just SE
                             "s"  => Just S
                             "sw" => Just SW
                             "w"  => Just W
                             "nw" => Just NW
                             "c"  => Just Center
                             "_"  => Just Underscore
                             _    => Nothing
                     _ => Nothing)

||| --
grEdgeOp : Grammar _ DOTToken True EdgeOp
grEdgeOp = terminal "Expected '--'"
            (\case GrEdgeOp => Just Dash
                   _ => Nothing)

||| ->
diGrEdgeOp : Grammar _ DOTToken True EdgeOp
diGrEdgeOp = terminal "Exepected '->'"
              (\case DiGrEdgeOp => Just Arrow
                     _ => Nothing)


-- Non-terminals --

||| Keywords ('node', 'edge', 'graph', 'digraph', 'subgraph', 'strict').
keyword : Grammar _ DOTToken True Keyword
keyword =  nodeKW
       <|> edgeKW
       <|> graphKW
       <|> digraphKW
       <|> subgraphKW
       <|> strictKW

||| An identifier is either:
||| - a name
||| - a numeral
||| - a quoted string
||| - an HTML string
identifier : Grammar _ DOTToken True DOTID
identifier =  nameID
          <|> numeralID
          <|> stringID
          <|> htmlID

||| Assignment, i.e.
||| ID '=' ID
assign_ : Grammar _ DOTToken True Assign
assign_ = do idLHS <- identifier
             equals
             idRHS <- identifier
             pure (MkAssign idLHS idRHS)

||| Separators are semicolons and commas, but they are purely aesthetic.
sepChoice : Grammar _ DOTToken False ()
sepChoice = ignore $ optional (choose semicolon comma)

||| An 'a_list' is an assignment, optionally followed by a separator, optionally
||| followed by more of an 'a_list'.
a_list : Grammar _ DOTToken True (List Assign)
a_list = do head <- assign_
            sepChoice
            rest <- (a_list <|> pure [])
            pure (head :: rest)

-- helper for `attr_list`
attr_list : Grammar _ DOTToken True (List (List Assign))
attr_list = do lBracket
               mAList <- optional a_list
               rBracket
               rest <- (attr_list <|> pure [])
               the (Grammar _ _ False _) $   -- case can confuse the type-checker
                 case mAList of
                      Nothing      => pure rest
                      (Just aList) => pure (aList :: rest)

||| An attr_stmt is one of the keywords 'graph', 'node', or 'edge', followed by
||| an attr_list.
attr_stmt : Grammar _ DOTToken True Stmt
attr_stmt =
  do kw <- gne    -- (graph|node|edge)
     attrList <- attr_list
     pure (AttrStmt kw attrList)
  where
    gne : Grammar _ DOTToken True Keyword
    gne =  graphKW
       <|> nodeKW
       <|> edgeKW

||| A colon followed by an ID, optionally followed by more colon+compass_pt
||| pairs.
idPort : Grammar _ DOTToken True Port
idPort = do colon
            id_ <- identifier
            maybeCPT <- optional compassPt
            pure (IDPort id_ maybeCPT)

||| A colon followed by a compass_pt.
cptPort : Grammar _ DOTToken True Port
cptPort = do colon
             cpt <- compassPt
             pure (PlainPort cpt)

||| A port is either:
||| - A colon followed by an ID, optionally followed by more colon+compass_pt
|||   pairs.
||| - A colon followed by a compass_pt.
port : Grammar _ DOTToken True Port
port =  idPort
    <|> cptPort

||| A 'node_id' is an identifier optionally followed by a port.
node_id : Grammar _ DOTToken True NodeID
node_id = do id_ <- identifier
             mPort <- optional port
             pure (MkNodeID id_ mPort)

||| A 'node_stmt' is a 'node_id' optionally followed by an 'attr_list'.
node_stmt : Grammar _ DOTToken True Stmt
node_stmt = do nID <- node_id
               attrList <- (attr_list <|> pure [])
               pure (NodeStmt nID attrList)

||| An edgeop is either '->' in directed graphs, or '--' in undirected graphs.
edgeop : Grammar _ DOTToken True EdgeOp
edgeop =  diGrEdgeOp
      <|> grEdgeOp

||| A subgraph start is the keyword 'subgraph' optionally followed by an
||| identifier.
subgraphStart : Grammar _ DOTToken True (Keyword, Maybe DOTID)
subgraphStart = do kw <- subgraphKW
                   mID <- optional identifier
                   pure (kw, mID)

mutual
  ||| A subgraph is optionally a `subgraphStart` (a helper function), followed
  ||| by a '{', followed by a 'stmt_list', followed by a '}'.
  subgraph : Grammar _ DOTToken True Subgraph
  subgraph = do start <- optional subgraphStart
                lBrace
                stmtList <- stmt_list
                rBrace
                pure (MkSubgraph start stmtList)

  -- helper for `edgeRHS` and 'edge_stmt'
  nidORsubgr : Grammar _ DOTToken True (Either NodeID Subgraph)
  nidORsubgr =  (do subgr <- subgraph; pure (Right subgr))
            <|> (do nID <- node_id; pure (Left nID))

  -- helper for `edgeRHS`, not requiring the list to be non-empty
  edgeRHS' : Grammar _ DOTToken True (List EdgeRHS)
  edgeRHS' = do edgeOp <- edgeop
                nORs <- nidORsubgr
                rest <- (edgeRHS' <|> pure [])
                pure ((MkEdgeRHS edgeOp nORs) :: rest)

  ||| The RHS of an edge is an 'edgeop', followed by either a 'node_id' or a
  ||| 'subgraph', optionally followed by more 'edgeRHS'.
  edgeRHS : Grammar _ DOTToken True (List1 EdgeRHS)
  edgeRHS = do edgeOp <- edgeop
               nORs <- nidORsubgr
               rest <- (edgeRHS' <|> pure [])
               pure ((MkEdgeRHS edgeOp nORs) ::: rest)

  ||| An 'edge_stmt' is either a 'node_id' or a 'subgraph', followed by an
  ||| 'edgeRHS', optionally followed by an 'attr_list'.
  edge_stmt : Grammar _ DOTToken True Stmt
  edge_stmt = do nORs <- nidORsubgr
                 rhs <- edgeRHS
                 attrList <- (attr_list <|> pure [])
                 pure (EdgeStmt nORs rhs attrList)

  subgr_stmt : Grammar _ DOTToken True Stmt
  subgr_stmt = do subgr <- subgraph
                  pure $ SubgraphStmt subgr

  assign_stmt : Grammar _ DOTToken True Stmt
  assign_stmt = do a <- assign_
                   pure $ AssignStmt a

  ||| A 'stmt' is either a 'node_stmt', 'edge_stmt', 'attr_stmt', an assignment,
  ||| or a subgraph.
  stmt : Grammar _ DOTToken True Stmt
  stmt =  subgr_stmt
      <|> edge_stmt
      <|> attr_stmt
      <|> assign_stmt
      <|> node_stmt

  ||| A 'stmt_list' is optionally: a 'stmt', followed by an optional semicolon,
  ||| followed by more of a 'stmt_list'.
  stmt_list : Grammar _ DOTToken True (List Stmt)
  stmt_list = do aStmt <- stmt
                 ignore $ optional semicolon   -- we don't store the ';'
                 rest <- (stmt_list <|> pure [])
                 pure (aStmt :: rest)

  -- Is the graph strict? Helper for 'graph'.
  isStrict : Grammar _ DOTToken False Bool
  isStrict = do (Just _) <- optional strictKW
                  | Nothing => pure False
                pure True

  -- Directed or undirected graph? Helper for 'graph'.
  graphType : Grammar _ DOTToken True Keyword
  graphType =  graphKW
           <|> digraphKW

  ||| A 'graph' is optionally the keyword "strict", followed by either the
  ||| keywords "graph" or "digraph", optionally followed by an identifier,
  ||| followed by a 'stmt_list' in braces.
  graph : Grammar _ DOTToken True Graph
  graph = do strict <- isStrict
             gType <- graphType
             mID <- optional identifier
             lBrace
             stmtList <- stmt_list
             rBrace
             pure (MkGraph (if strict then Just StrictKW else Nothing) gType mID stmtList)

export
parse :  (xs : List (WithBounds DOTToken))
      -> Either (List1 (ParsingError DOTToken)) (Graph, List (WithBounds DOTToken))
parse xs = parse graph xs

