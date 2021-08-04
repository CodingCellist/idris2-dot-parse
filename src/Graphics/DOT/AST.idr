||| Types for representing a DOT file/graph in Idris
module Graphics.DOT.AST

import Data.Vect

{- TODO:
 - [x] How to best split things into types?
   * Answer: don't do so. Worry about getting things working first.
 - [x] Should each keyword have its own constructor (gut instinct: yes)
   * At least for now (see previous point).
 - [x] ~~Use `Maybe` for optional keywords/modifiers~~
   * [x] Nope, `List DOT` seems to work better.
 - [x] Identifiers
 ... the rest of this todo-list ...
 - [ ] Nice type-level functions to avoid having to specify `Nothing` all the
       time
 -}

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

  -- a_list AST node, takes list of stuff
  AList : List DOT -> DOT   -- non-empty?

  -- attr_list AST node; should a_list look similar?
  AttrList : List DOT -> DOT

  -- Operators
  -- TODO: split things into separate types to limit what can go where?
  Assign : Vect 2 DOT -> DOT
  StrConcat : Vect (S (S k)) DOT -> DOT   -- must have at least 2 strings
  DiEdgeOp : Vect 2 DOT -> DOT
  EdgeOp : Vect 2 DOT -> DOT

  -- Ports
  IDPort  : (id : DOT) -> (c_pt : Maybe DOT) -> DOT
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


