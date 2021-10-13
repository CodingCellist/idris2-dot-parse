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


export
Show DOT where
  show (Graph strict type id_ stmtList) =
    "(Gr " ++ show strict ++ show type ++ show id_ ++ show stmtList
  show (StmtList xs) = show xs
  show (Stmt stmt) = show stmt
  show Node = "Node"
  show Edge = "Edge"
  show GraphKW = "GrKW"
  show DiGraph = "DiGr"
  show SubGraph = "SuGr"
  show Strict = "Strict"
  show (NameID name) = "(Name " ++ name ++ ")"
  show (NumeralID numeral) = "(Nume " ++ numeral ++ ")"
  show (StringID str) = "(StrI " ++ str ++ ")"
  show (HTML_ID html) = "(HTML " ++ html ++ ")"
  show (AList xs) = "(AList:=" ++ show xs ++ ")"
  show (AttrList xs) = "(AttrList:=" ++ show xs ++ ")"
  show (AttrStmt kw attrList) = "(AttrStmt " ++ show kw ++ show attrList ++ ")"
  show (Assign [a, b]) = "(Assign " ++ show a ++ show b ++ ")"
  show (StrConcat xs) = "(StrConc " ++ show xs ++ ")"
  show DiGrEdgeOp = "-->"
  show GrEdgeOp = "---"
  show (EdgeRHS xs) = "(ERhs " ++ show xs ++ ")"
  show (EdgeStmt x rhs attrList) =
    "(EStmt " ++ show x ++ show rhs ++ show attrList ++ ")"
  show (NodeID id_ port) =
    "(NodeID " ++ show id_ ++ (case port of Nothing => ""; Just p => ":" ++ show p) ++ ")"
  show (NodeStmt nID attrList) = "(NodeStmt " ++ show nID ++ show attrList ++ ")"
  show (IDPort id_ c_pt) =
    "(IDPort " ++ show id_ ++ (case c_pt of Nothing => ""; Just pt => ":" ++ show pt) ++ ")"
  show (CPTPort c_pt) = show c_pt
  show (SubgraphID id_) = show id_
  show (Subgraph sID stmtList) = "(Subgraph " ++ show sID ++ show stmtList ++ ")"
  show North = "North"
  show NorthEast = "NorthEast"
  show East = "East"
  show SouthEast = "SouthEast"
  show South = "South"
  show SouthWest = "SouthWest"
  show West = "West"
  show NorthWest = "NorthWest"
  show CenterCPt = "Center"
  show UnderCPt = "_DEF_"

