||| Types for representing a DOT file/graph in Idris
module Graphics.DOT.AST

import Data.Vect

||| The stuff in the AST of a DOT graph
public export
data DOT : Type where
  -- Graphs - The bit where everything starts
  Graph :  (strict : Bool)
        -> (type : DOT)
        -> (id_ : Maybe DOT)
        -> (stmtList : DOT)
        -> DOT

  -- Statements
  StmtList : List DOT -> DOT
  Stmt : (stmt : DOT) -> DOT

  -- Keywords
  Node : DOT
  Edge : DOT
  GraphKW : DOT
  DiGraph : DOT
  SubGraph : DOT
  Strict : DOT

  -- Identifiers
  NameID : (name : String) -> DOT
  NumeralID : (numeral : String) -> DOT
  StringID : (str : String) -> DOT
  HTML_ID : (html : String) -> DOT

  -- Attributes
  -- a_list AST node, takes list of stuff
  AList : List DOT -> DOT   -- non-empty?
  AttrList : List DOT -> DOT
  AttrStmt : (kw : DOT) -> (attrList : DOT) -> DOT

  -- Operators
  Assign : (ids : Vect 2 DOT) -> DOT
  StrConcat : Vect (S (S k)) DOT -> DOT   -- must have at least 2 strings
  -- unsure if this is would be better: Vect 2 DOT -> DOT
  DiGrEdgeOp : DOT
  GrEdgeOp : DOT    

  -- Edges
  -- unsure about this one...
  -- EdgeRHS : (edgeOp : DOT) -> DOT -> (moreRHS : List DOT) -> DOT
  -- EdgeRHS : List (DOT, DOT) -> DOT   -- this might be better?
  EdgeRHS : List DOT -> DOT
  EdgeStmt : DOT -> (rhs : DOT) -> (attrList : Maybe DOT) -> DOT

  -- Nodes
  NodeID : (id_ : DOT) -> (port : Maybe DOT) -> DOT
  NodeStmt : (nID : DOT) -> (attrList : Maybe DOT) -> DOT

  -- Ports
  IDPort  : (id_ : DOT) -> (c_pt : Maybe DOT) -> DOT
  CPTPort : (c_pt : DOT) -> DOT

  -- Subgraphs
  SubgraphID : (id_ : Maybe DOT) -> DOT
  Subgraph : (sID : Maybe DOT) -> (stmtList : DOT) -> DOT

  -- Compass points
  North : DOT
  NorthEast : DOT
  East : DOT 
  SouthEast : DOT
  South : DOT
  SouthWest : DOT
  West : DOT
  NorthWest : DOT
  CenterCPt : DOT
  UnderCPt : DOT


