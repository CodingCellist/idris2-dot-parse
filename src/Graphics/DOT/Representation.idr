||| Types for representing a DOT file/graph in Idris
module Graphics.DOT.Representation

import Data.Vect

{- TODO:
 - [ ] How to best split things into types?
 - [ ] Should each keyword have its own constructor (gut instinct: yes)
 - [ ] Use `Maybe` for optional keywords/modifiers
 - [ ] Identifiers
 ... the rest of this todo-list ...
 - [ ] Nice type-level functions to avoid having to specify `Nothing` all the
       time
 -}

||| A DOT graph
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

  -- a_list AST node, takes list of stuff
  AList : List DOT -> DOT   -- non-empty?

  -- Operators
  -- TODO: split things into separate types to limit what can go where?
  Assign : Vect 2 DOT -> DOT
  StrConcat : Vect (S (S k)) DOT -> DOT   -- must have at least 2 strings
  DiEdgeOp : Vect 2 DOT -> DOT
  EdgeOp : Vect 2 DOT -> DOT

  Colon : DOT

  -- Ports
  IDPort  : (colon : DOT) -> (id : DOT) -> (c_pt : Maybe DOT) -> DOT
  CPTPort : (colon : DOT) -> (c_pt : DOT) -> DOT


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


