module Graphics.DOT.Parser

import Text.Parser
import Data.String
import Data.Vect

import Graphics.DOT.Lexer
import Graphics.DOT.AST

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

nameID : Grammar _ DOTToken True DOT
nameID = terminal "Not a name"
          (\case (NameID str) => Just (NameID str)
                 _ => Nothing)

numeralID : Grammar _ DOTToken True DOT
numeralID = terminal "Not a numeral"
              (\case (NumeralID num) => Just (NumeralID num)
                     _ => Nothing)

stringID : Grammar _ DOTToken True DOT
stringID = terminal "Not a string"
            (\case (StringID str) => Just (StringID str)
                   _ => Nothing)

htmlID : Grammar _ DOTToken True DOT
htmlID = terminal "Not an HTML string"
          (\case (HTML_ID html) => Just (HTML_ID html)
                 _ => Nothing)

nodeKW : Grammar _ DOTToken True DOT
nodeKW = terminal "Expected 'node' keyword"
          (\case Keyword "node" => Just Node
                 _ => Nothing)

edgeKW : Grammar _ DOTToken True DOT
edgeKW = terminal "Expecetd 'edge' keyword"
          (\case Keyword "edge" => Just Edge
                 _ => Nothing)

graphKW : Grammar _ DOTToken True DOT
graphKW = terminal "Expected 'graph' keyword"
           (\case Keyword "graph" => Just GraphKW
                  _ => Nothing)

digraphKW : Grammar _ DOTToken True DOT
digraphKW = terminal "Expected 'digraph' keyword"
              (\case Keyword "digraph" => Just DiGraph
                     _ => Nothing)

subgraphKW : Grammar _ DOTToken True DOT
subgraphKW = terminal "Expected 'subgraph' keyword"
              (\case Keyword "subgraph" => Just SubGraph
                     _ => Nothing)

strictKW : Grammar _ DOTToken True DOT
strictKW = terminal "Expected 'strict' keyword"
            (\case Keyword "strict" => Just Strict
                   _ => Nothing)

||| Compass points (n, ne, e, se, s, sw, w, nw, c, _).
compassPt : Grammar _ DOTToken True DOT
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

||| --
grEdgeOp : Grammar _ DOTToken True DOT
grEdgeOp = terminal "Expected '--'"
            (\case GrEdgeOp => Just GrEdgeOp
                   _ => Nothing)

||| ->
diGrEdgeOp : Grammar _ DOTToken True DOT
diGrEdgeOp = terminal "Exepected '->'"
              (\case DiGrEdgeOp => Just DiGrEdgeOp
                     _ => Nothing)


-- Non-terminals --

||| Keywords ('node', 'edge', 'graph', 'digraph', 'subgraph', 'strict').
keyword : Grammar _ DOTToken True DOT
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
identifier : Grammar _ DOTToken True DOT
identifier =  nameID
          <|> numeralID
          <|> stringID
          <|> htmlID

||| Assignment, i.e.
||| ID '=' ID
assign_ : Grammar _ DOTToken True DOT
assign_ = do idLHS <- identifier
             equals
             idRHS <- identifier
             pure (Assign [idLHS, idRHS])    -- returns assign node in AST

||| Separators are semicolons and commas, but they are purely aesthetic.
sepChoice : Grammar _ DOTToken False ()
sepChoice = ignore $ optional (choose semicolon comma)

-- helper for `a_list`
a_list' : Grammar _ DOTToken True (List DOT)
a_list' = do head <- assign_
             sepChoice
             rest <- (a_list' <|> pure [])
             pure (head :: rest)

||| An 'a_list' is an assignment, optionally followed by a separator, optionally
||| followed by more of an 'a_list' (see helper `a_list'`).
a_list : Grammar _ DOTToken True DOT
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

-- helper for `attr_list`
attr_list' : Grammar _ DOTToken True (List DOT)
attr_list' = do lBracket
                mAList <- optional a_list
                rBracket
                rest <- (attr_list' <|> pure [])
                the (Grammar _ _ False _) $   -- case can confuse the type-checker
                  case mAList of
                       Nothing      => pure rest
                       (Just aList) => pure (aList :: rest)

||| An 'attr_list' is a '[', optionally followed by an 'a_list', followed by a
||| ']', optionally followed by another 'attr_list' (see helper `attr_list'`).
attr_list : Grammar _ DOTToken True DOT
attr_list = do l <- attr_list'
               pure (AttrList l)

-- OLD IMPLEMENTATION:
--attr_list =  (do lBracket
--                 mAList <- optional a_list
--                 rBracket
--                 pure (AttrList (maybeToList mAList)))
--         <|> (do lBracket
--                 mAList <- optional a_list
--                 rBracket
--                 rest <- attr_list
--                 pure (AttrList ((maybeToList mAList) ++ [rest])))
--          where
--            maybeToList : Maybe DOT -> List DOT
--            maybeToList Nothing    = []
--            maybeToList (Just dot) = [dot]

||| An attr_stmt is one of the keywords 'graph', 'node', or 'edge', followed by
||| an attr_list.
attr_stmt : Grammar _ DOTToken True DOT
attr_stmt =
  do kw <- gne    -- (graph|node|edge)
     attrList <- attr_list
     pure (AttrStmt kw attrList)
  where
    gne : Grammar _ DOTToken True DOT
    gne =  graphKW
       <|> nodeKW
       <|> edgeKW

||| A colon followed by an ID, optionally followed by more colon+compass_pt
||| pairs.
idPort : Grammar _ DOTToken True DOT
idPort = do colon
            id_ <- identifier
            maybeCPT <- optional compassPt
            pure (IDPort id_ maybeCPT)

||| A colon followed by a compass_pt.
cptPort : Grammar _ DOTToken True DOT
cptPort = do colon
             cpt <- compassPt
             pure (CPTPort cpt)

||| A port is either:
||| - A colon followed by an ID, optionally followed by more colon+compass_pt
|||   pairs.
||| - A colon followed by a compass_pt.
port : Grammar _ DOTToken True DOT
port =  idPort
    <|> cptPort

||| A 'node_id' is an identifier optionally followed by a port.
node_id : Grammar _ DOTToken True DOT
node_id = do id_ <- identifier
             mPort <- optional port
             pure (NodeID id_ mPort)

||| A 'node_stmt' is a 'node_id' optionally followed by an 'attr_list'.
node_stmt : Grammar _ DOTToken True DOT
node_stmt = do nID <- node_id
               attrList <- optional attr_list
               pure (NodeStmt nID attrList)

||| An edgeop is either '->' in directed graphs, or '--' in undirected graphs.
edgeop : Grammar _ DOTToken True DOT
edgeop =  diGrEdgeOp
      <|> grEdgeOp

||| A subgraph ID is the keyword 'subgraph' optionally followed by an
||| identifier.
subgraphID : Grammar _ DOTToken True DOT
subgraphID = do ignore $ subgraphKW   -- only one possible kw, so don't store it
                mID <- optional identifier
                pure (SubgraphID mID)

mutual
  ||| A subgraph is optionally a `subgraphID` (a helper function), followed by a
  ||| '{', followed by a 'stmt_list', followed by a '}'.
  subgraph : Grammar _ DOTToken True DOT
  subgraph = do sID <- optional subgraphID
                lBrace
                stmtList <- stmt_list
                rBrace
                pure (Subgraph sID stmtList)

  -- helper for `edgeRHS'` (which is itself a helper) and 'edge_stmt'
  nidORsubgr : Grammar _ DOTToken True DOT
  nidORsubgr =  subgraph
            <|> node_id

  -- helper for edgeRHS
  edgeRHS' : Grammar _ DOTToken True (List DOT)
  edgeRHS' = do edgeOp <- edgeop
                nORs <- nidORsubgr
                rest <- (edgeRHS' <|> pure [])
                pure (edgeOp :: nORs :: rest)

  ||| The RHS of an edge is an 'edgeop', followed by either a 'node_id' or a
  ||| 'subgraph', optionally followed by more 'edgeRHS'.
  edgeRHS : Grammar _ DOTToken True DOT
  edgeRHS = do l <- edgeRHS'
               pure (EdgeRHS l)

  ||| An 'edge_stmt' is either a 'node_id' or a 'subgraph', followed by an
  ||| 'edgeRHS', optionally followed by an 'attr_list'.
  edge_stmt : Grammar _ DOTToken True DOT
  edge_stmt = do nORs <- nidORsubgr
                 rhs <- edgeRHS
                 mAttrList <- optional attr_list
                 pure (EdgeStmt nORs rhs mAttrList)

  -- The possible things that can be in a statement. A helper for 'stmt'.
  stmtAlts : Grammar _ DOTToken True DOT
  stmtAlts =  subgraph
          <|> edge_stmt
          <|> attr_stmt
          <|> assign_
          <|> node_stmt

  ||| A 'stmt' is either a 'node_stmt', 'edge_stmt', 'attr_stmt', an assignment,
  ||| or a subgraph.
  stmt : Grammar _ DOTToken True DOT
  stmt = do theStmt <- stmtAlts
            pure (Stmt theStmt)

  -- helper for 'stmt_list'
  stmt_list' : Grammar _ DOTToken True (List DOT)
  stmt_list' = do aStmt <- stmt
                  ignore $ optional semicolon   -- we don't store the ';'
                  rest <- (stmt_list' <|> pure [])
                  pure (aStmt :: rest)

  ||| A 'stmt_list' is optionally: a 'stmt', followed by an optional semicolon,
  ||| followed by more of a 'stmt_list' (see the `stmt_list'` helper).
  stmt_list : Grammar _ DOTToken True DOT
  stmt_list = do theStmts <- stmt_list'
                 pure (StmtList theStmts)

  -- Is the graph strict? Helper for 'graph'.
  isStrict : Grammar _ DOTToken False Bool
  isStrict = do (Just _) <- optional strictKW
                  | Nothing => pure False
                pure True

  -- Directed or undirected graph? Helper for 'graph'.
  graphType : Grammar _ DOTToken True DOT
  graphType =  graphKW
           <|> digraphKW

  ||| A 'graph' is optionally the keyword "strict", followed by either the
  ||| keywords "graph" or "digraph", optionally followed by an identifier,
  ||| followed by a 'stmt_list' in braces.
  graph : Grammar _ DOTToken True DOT
  graph = do strict <- isStrict
             gType <- graphType
             mID <- optional identifier
             lBrace
             stmtList <- stmt_list
             rBrace
             pure (Graph strict gType mID stmtList)

export
parse :  (xs : List (WithBounds DOTToken))
      -> Either (List1 (ParsingError DOTToken)) (DOT, List (WithBounds DOTToken))
parse xs = parse graph xs

