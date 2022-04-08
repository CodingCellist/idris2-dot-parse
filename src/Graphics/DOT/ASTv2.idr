module Graphics.DOT.ASTv2

import Data.List1

%default total

||| The various compass points that can be used in DOT
public export
data CompassPoint
  = N
  | NE
  | E
  | SE
  | S
  | SW
  | W
  | NW
  | Center
  | Underscore

||| The various DOT keywords
public export
data Keyword
  = StrictKW
  | GraphKW
  | DigraphKW
  | NodeKW
  | EdgeKW
  | SubgraphKW

||| A DOT identifier
public export
data DOTID : Type where
  StringID : (id_ : String) -> DOTID
  NameID   : (name : String) -> DOTID
  Numeral  : (num : String) -> DOTID
  HTML     : (htmlStr : String) -> DOTID

||| A DOT port
public export
data Port : Type where
  ||| A port with an id
  |||   port :=  ':' ID [ ':' compass_pt ]
  IDPort :  (id_ : DOTID) -> Maybe CompassPoint -> Port

  ||| A port without an id
  |||   port |=  ':' compass_pt
  PlainPort : CompassPoint -> Port

||| A node identifier (separate from regular identifiers, for some reason)
public export
data NodeID : Type where
  MkNodeID : (id_ : DOTID) -> (mPort : Maybe Port) -> NodeID

||| Assignment
|||   id '=' id
public export
data Assign : Type where
  MkAssign :  (lhs : DOTID)
           -> (rhs : DOTID)
           -> Assign

||| An edge op
public export
data EdgeOp : Type where
  -- TODO: Could be dependent on whether the graph is `strict`?
  Arrow : EdgeOp
  Dash  : EdgeOp

mutual
  ||| The right hand side (RHS) of an edge op
  public export
  data EdgeRHS : Type where
    MkEdgeRHS :  (op : EdgeOp)
              -> Either NodeID Subgraph
              -> EdgeRHS

  ||| A subgraph
  public export
  data Subgraph : Type where
    MkSubgraph :  Maybe (Keyword, Maybe DOTID)
               -> (stmtList : List Stmt)
               -> Subgraph

  ||| A DOT statement
  public export
  data Stmt : Type where

    NodeStmt : (nodeID : NodeID) -> (attrList : List (List Assign)) -> Stmt

    EdgeStmt :  Either NodeID Subgraph
             -> (rhs : List1 EdgeRHS)
             -> (attrList : List (List Assign))
             -> Stmt

    ||| An attribute statement
    ||| `attr_stmt : (graph | node | edge) '[' [ a_list ] ']' [ attr_list ]`
    AttrStmt :  (kw : Keyword)
             -> (attrList : List (List Assign))
             -> Stmt

    AssignStmt : (a : Assign) -> Stmt

    SubgraphStmt : (subGr : Subgraph) -> Stmt

||| A DOT graph. This is the top-level IR node
public export
data Graph : Type where
  -- TODO: this could possibly be turned into multiple constructors
  --       (e.g. Graph, Digraph, StrictGraph, etc.)?
  MkGraph :  (strict : Maybe Keyword)
          -> (graphTy : Keyword)
          -> (mID_ : Maybe DOTID)
          -> (stmtList : List Stmt)
          -> Graph

