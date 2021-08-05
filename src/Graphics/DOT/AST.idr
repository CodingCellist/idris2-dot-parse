||| Types for representing a DOT file/graph in Idris
module Graphics.DOT.AST

import Data.Vect

||| The stuff in the AST of a DOT graph
public export
data DOT : Type where
  -- Keywords
  Node : DOT
  Edge : DOT
  Graph : DOT
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
  -- TODO: split things into separate types to limit what can go where?
  Assign : (ids : Vect 2 DOT) -> DOT
  StrConcat : Vect (S (S k)) DOT -> DOT   -- must have at least 2 strings
  DiEdgeOp : Vect 2 DOT -> DOT
  EdgeOp : Vect 2 DOT -> DOT

  NodeID : (id_ : DOT) -> (port : Maybe DOT) -> DOT
  NodeStmt : (nID : DOT) -> (attrList : Maybe DOT) -> DOT


  -- Ports
  IDPort  : (id_ : DOT) -> (c_pt : Maybe DOT) -> DOT
  CPTPort : (c_pt : DOT) -> DOT

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


